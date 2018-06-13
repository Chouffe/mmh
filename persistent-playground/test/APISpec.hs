{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Database              (fetchRedisInfo, fetchSQLiteInfo)

import           Control.Concurrent    (killThread)
import           Control.Monad         (forM_, void)
import           Data.Either           (isLeft, isRight)
import           Data.Int              (Int64)
import           Data.Maybe            (isJust)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Database.Persist.Sql  (Entity (..), entityVal, fromSqlKey,
                                        toSqlKey)
import           Servant.Client        (ClientEnv, runClientM)
import           Test.Hspec

import           API                   (createArticleClient, createUserClient,
                                        fetchArticleByAuthorClient,
                                        fetchArticleClient,
                                        fetchRecentArticlesClient,
                                        fetchUserClient)
import           Database              (RedisInfo, SQLiteInfo,
                                        createArticleSQLite, createUserSQLite,
                                        deleteArticleSQLite, deleteUserCache,
                                        deleteUserSQLite, fetchArticleSQLite,
                                        fetchUserRedis, fetchUserSQLite)
import           Schema                (Article (..), User (..))
import           TestUtils             (setupTests)


main :: IO ()
main = do
  -- Setting up test server and config
  (sqliteInfo, redisInfo, clientEnv, tid) <- setupTests

  hspec
    $ before (beforeHook1 clientEnv sqliteInfo redisInfo)
    $ spec1

  hspec
    $ before (beforeHook2 clientEnv sqliteInfo redisInfo)
    $ after (afterHook sqliteInfo redisInfo)
    $ spec2

  hspec
    $ before (beforeHook3 clientEnv sqliteInfo redisInfo)
    $ after (afterHook sqliteInfo redisInfo)
    $ spec3

  hspec
    $ before (beforeHook4 clientEnv sqliteInfo)
    $ after (afterHook4 sqliteInfo redisInfo)
    $ spec4

  hspec
    $ before (beforeHook5 clientEnv sqliteInfo)
    $ after (afterHook5 sqliteInfo redisInfo)
    $ spec5

  hspec
    $ before (beforeHook6 clientEnv sqliteInfo)
    $ after (afterHook6 sqliteInfo redisInfo)
    $ spec6

  -- Killing test server
  killThread tid

  return ()

beforeHook1 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO (Bool, Bool, Bool)
beforeHook1 clientEnv sqliteInfo redisInfo = do
  callResult <- runClientM (fetchUserClient userid) clientEnv
  inDB       <- isJust <$> fetchUserSQLite sqliteInfo userid
  inRedis    <- isJust <$> fetchUserRedis redisInfo userid
  let throwsError = isLeft callResult
  return (throwsError, inDB, inRedis)

  where
    userid :: Int64
    userid = 1

spec1 :: SpecWith (Bool, Bool, Bool)
spec1 = describe "After fetching on an empty database" $ do
  it "The fetch call should throw an error" $ \(throwsError, _, _) -> throwsError `shouldBe` True
  it "There should be no user in DB" $ \(_, inDB, _) -> inDB `shouldBe` False
  it "There should be no user in cache" $ \(_, _, inRedis) -> inRedis `shouldBe` False

beforeHook2 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook2 clientEnv sqliteInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _        -> error "DB called failed on spec2"
    Right userKey -> do
      inDB    <- isJust <$> fetchUserSQLite sqliteInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inDB, inRedis, userKey)

  where
    testUser :: User
    testUser = User "Test User" "test.user@test.test" 21 "SWE"

afterHook :: SQLiteInfo -> RedisInfo -> (Bool, Bool, Int64) -> IO ()
afterHook sqliteInfo redisInfo (_, _, key) = do
  deleteUserCache redisInfo key
  deleteUserSQLite sqliteInfo key

spec2 :: SpecWith (Bool, Bool, Int64)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in DB" $ \(inDB, _, _) -> inDB `shouldBe` True
  it "There should not be a user in Cache" $ \(_, inRedis, _) -> inRedis `shouldBe` False

beforeHook3 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook3 clientEnv sqliteInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  case userKeyEither of
    Left _ -> error "DB called failed on spec3"
    Right userKey -> do
      _       <- runClientM (fetchUserClient userKey) clientEnv
      inDB    <- isJust <$> fetchUserSQLite sqliteInfo userKey
      inRedis <- isJust <$> fetchUserRedis redisInfo userKey
      return (inDB, inRedis, userKey)

  where
    testUser :: User
    testUser = User "Test User" "test.user@test.test" 21 "SWE"

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in DB" $ \(inDB, _, _) -> inDB `shouldBe` True
  it "There should be a user in Cache" $ \(_, inRedis, _) -> inRedis `shouldBe` True

deleteArtifacts :: SQLiteInfo -> RedisInfo -> [Int64] -> [Int64] -> IO ()
deleteArtifacts sqliteInfo redisInfo users articles = do
  -- Delete the articles first because of foreign keys
  forM_ articles $ \a -> deleteArticleSQLite sqliteInfo a
  forM_ users $ \u -> do
    deleteUserCache redisInfo u
    deleteUserSQLite sqliteInfo u

beforeHook4 :: ClientEnv -> SQLiteInfo -> IO (Bool, Bool, Int64, Int64)
beforeHook4 clientEnv sqliteInfo = do
  userKey <- createUserSQLite sqliteInfo testUser2
  articleKeyEither <- runClientM (createArticleClient (testArticle1 userKey)) clientEnv
  case articleKeyEither of
    Left _ -> error "DB call failed on spec 4!"
    Right articleKey -> do
      fetchResult <- runClientM (fetchArticleClient articleKey) clientEnv
      let callSucceeds = isRight fetchResult
      articleInDB <- isJust <$> fetchArticleSQLite sqliteInfo articleKey
      return (callSucceeds, articleInDB, userKey, articleKey)

spec4 :: SpecWith (Bool, Bool, Int64, Int64)
spec4 = describe "After createing a user and an article" $ do
  it "The call should return a result" $ \(succeeds, _, _, _) -> succeeds `shouldBe` True
  it "The article should be in DB" $ \(_, inDB, _, _) -> inDB `shouldBe` True

afterHook4 :: SQLiteInfo -> RedisInfo -> (Bool, Bool, Int64, Int64) -> IO ()
afterHook4 sqliteInfo redisInfo (_, _, userid, articleid) = deleteArtifacts sqliteInfo redisInfo [userid] [articleid]

beforeHook5 :: ClientEnv -> SQLiteInfo -> IO ([Article], [Article], Int64, Int64, [Int64])
beforeHook5 clientEnv sqliteInfo = do
  uid1       <- createUserSQLite sqliteInfo testUser3
  uid2       <- createUserSQLite sqliteInfo testUser4
  articleIds <- mapM (createArticleSQLite sqliteInfo)
    [ testArticle2 uid1, testArticle3 uid1, testArticle4 uid1
    , testArticle5 uid2, testArticle6 uid2
    ]
  firstArticles  <- runClientM (fetchArticleByAuthorClient uid1) clientEnv
  secondArticles <- runClientM (fetchArticleByAuthorClient uid2) clientEnv
  case (firstArticles, secondArticles) of
    (Right as1, Right as2) -> return (entityVal <$> as1, entityVal <$> as2, uid1, uid2, articleIds)
    _                      -> error "Spec 5 failed!"

afterHook5 :: SQLiteInfo -> RedisInfo -> ([Article], [Article], Int64, Int64, [Int64]) -> IO ()
afterHook5 sqliteInfo redisInfo (_, _, uid1, uid2, articleIds) = deleteArtifacts sqliteInfo redisInfo [uid1, uid2] articleIds

spec5 :: SpecWith ([Article], [Article], Int64, Int64, [Int64])
spec5 = describe "When fetching articles by author id" $ do
  it "Fetching by the first author should return 3 articles" $ \(firstArticles, _, _, _, _) -> length firstArticles `shouldBe` 3
  it "Fetching by the second author should return 2 articles" $ \(_, secondArticles, _, _, _) -> length secondArticles `shouldBe` 2

beforeHook6 :: ClientEnv -> SQLiteInfo -> IO ([(User, Article)], Int64, Int64, [Int64])
beforeHook6 clientEnv sqliteInfo = do
  uid1 <- createUserSQLite sqliteInfo testUser5
  uid2 <- createUserSQLite sqliteInfo testUser6
  articleIds <- mapM (createArticleSQLite sqliteInfo)
    [ testArticle7 uid1, testArticle8 uid1, testArticle9 uid1, testArticle10 uid2
    , testArticle11 uid2, testArticle12 uid1, testArticle13 uid2, testArticle14 uid2
    , testArticle15 uid2, testArticle16 uid1, testArticle17 uid1, testArticle18 uid2
    ]
  recentArticles <- runClientM fetchRecentArticlesClient clientEnv
  case recentArticles of
    Right as -> return (entityValTuple <$> as, uid1, uid2, articleIds)
    _        -> error "Spec 6 failed!"
  where
    entityValTuple :: (Entity User, Entity Article) -> (User, Article)
    entityValTuple (eUser, eArticle) = (entityVal eUser, entityVal eArticle)

afterHook6 :: SQLiteInfo -> RedisInfo -> ([(User, Article)], Int64, Int64, [Int64]) -> IO ()
afterHook6 sqliteInfo redisInfo (_, uid1, uid2, articleIds) = deleteArtifacts sqliteInfo redisInfo [uid1, uid2] articleIds

spec6 :: SpecWith ([(User, Article)], Int64, Int64, [Int64])
spec6 = describe "When fetching recent articles" $ do
  it "There should be only 10 articles" $ \(xs, _, _, _) -> length xs `shouldBe` 10

-- Test Data

testUser1 :: User
testUser1 = User
  { userName = "james"
  , userEmail = "james@test.com"
  , userAge = 25
  , userOccupation = "Software Engineer"
  }

testUser2 :: User
testUser2 = User
  { userName = "kate"
  , userEmail = "kate@test.com"
  , userAge = 24
  , userOccupation = "Software Engineer"
  }

testUser3 :: User
testUser3 = User
  { userName = "jeremy"
  , userEmail = "jeremy@test.com"
  , userAge = 23
  , userOccupation = "Teacher"
  }

testUser4 :: User
testUser4 = User
  { userName = "alex"
  , userEmail = "alex@test.com"
  , userAge = 30
  , userOccupation = "Petroleum Engineer"
  }

testUser5 :: User
testUser5 = User
  { userName = "adam"
  , userEmail = "adam@test.com"
  , userAge = 30
  , userOccupation = "Accountant"
  }

testUser6 :: User
testUser6 = User
  { userName = "alexa"
  , userEmail = "alexa@test.com"
  , userAge = 30
  , userOccupation = "Mechanical Engineer"
}
testArticle1 :: Int64 -> Article
testArticle1 uid = Article
  { articleTitle = "First post"
  , articleBody = "A great description of our first blog post body."
  , articlePublishedTime = posixSecondsToUTCTime 1498914000
  , articleAuthorId = toSqlKey uid
  }

testArticle2 :: Int64 -> Article
testArticle2 uid = Article
  { articleTitle = "Second post"
  , articleBody = "Dummy body description"
  , articlePublishedTime = posixSecondsToUTCTime 1498917600
  , articleAuthorId = toSqlKey uid
  }

testArticle3 :: Int64 -> Article
testArticle3 uid = Article
  { articleTitle = "Third post"
  , articleBody = "Fascinating!"
  , articlePublishedTime = posixSecondsToUTCTime 1498921200
  , articleAuthorId = toSqlKey uid
  }

testArticle4 :: Int64 -> Article
testArticle4 uid = Article
  { articleTitle = "Fourth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924800
  , articleAuthorId = toSqlKey uid
  }

testArticle5 :: Int64 -> Article
testArticle5 uid = Article
  { articleTitle = "Fifth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928400
  , articleAuthorId = toSqlKey uid
  }

testArticle6 :: Int64 -> Article
testArticle6 uid = Article
  { articleTitle = "Sixth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932000
  , articleAuthorId = toSqlKey uid
  }

testArticle7 :: Int64 -> Article
testArticle7 uid = Article
  { articleTitle = "Seventh post"
  , articleBody = "A great description of our first blog post body."
  , articlePublishedTime = posixSecondsToUTCTime 1498914000
  , articleAuthorId = toSqlKey uid
  }

testArticle8 :: Int64 -> Article
testArticle8 uid = Article
  { articleTitle = "Eighth post"
  , articleBody = "Dummy body description"
  , articlePublishedTime = posixSecondsToUTCTime 1498917600
  , articleAuthorId = toSqlKey uid
  }

testArticle9 :: Int64 -> Article
testArticle9 uid = Article
  { articleTitle = "Ninth post"
  , articleBody = "Fascinating!"
  , articlePublishedTime = posixSecondsToUTCTime 1498921200
  , articleAuthorId = toSqlKey uid
  }

testArticle10 :: Int64 -> Article
testArticle10 uid = Article
  { articleTitle = "Tenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924800
  , articleAuthorId = toSqlKey uid
  }

testArticle11 :: Int64 -> Article
testArticle11 uid = Article
  { articleTitle = "Eleventh post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928400
  , articleAuthorId = toSqlKey uid
  }

testArticle12 :: Int64 -> Article
testArticle12 uid = Article
  { articleTitle = "Twelfth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932000
  , articleAuthorId = toSqlKey uid
  }

testArticle13 :: Int64 -> Article
testArticle13 uid = Article
  { articleTitle = "Thirteenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498914001
  , articleAuthorId = toSqlKey uid
  }

testArticle14 :: Int64 -> Article
testArticle14 uid = Article
  { articleTitle = "Fourteenth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498917601
  , articleAuthorId = toSqlKey uid
  }

testArticle15 :: Int64 -> Article
testArticle15 uid = Article
  { articleTitle = "Fifteenth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498921201
  , articleAuthorId = toSqlKey uid
  }

testArticle16 :: Int64 -> Article
testArticle16 uid = Article
  { articleTitle = "Sixteenth post"
  , articleBody = "Overall summary of the blog"
  , articlePublishedTime = posixSecondsToUTCTime 1498924801
  , articleAuthorId = toSqlKey uid
  }

testArticle17 :: Int64 -> Article
testArticle17 uid = Article
  { articleTitle = "Seventeenth post"
  , articleBody = "Description of everything in the universe"
  , articlePublishedTime = posixSecondsToUTCTime 1498928401
  , articleAuthorId = toSqlKey uid
  }

testArticle18 :: Int64 -> Article
testArticle18 uid = Article
  { articleTitle = "Eighteenth post"
  , articleBody = "A reflection on the events of the past week"
  , articlePublishedTime = posixSecondsToUTCTime 1498932001
  , articleAuthorId = toSqlKey uid
}
