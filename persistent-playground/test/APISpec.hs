{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent      (killThread)
import           Control.Concurrent.MVar (MVar, readMVar)
import           Control.Exception       (SomeException)
import           Control.Monad           (forM_, void)
import           Control.Monad.Freer     (Eff, Member)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Either             (isLeft, isRight)
import           Data.Int                (Int64)
import           Data.Maybe              (isJust)
import           Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import           Database.Persist.Sql    (Entity (..), entityVal, fromSqlKey,
                                          toSqlKey)
import           Servant.Client          (ClientEnv, runClientM)
import           Test.Hspec


import           API
import           Cache
import           Database
import qualified Eff.Cache               as EffCache
import qualified Eff.Database            as EffDatabase
import           Monad.App
import           Monad.Cache
import           Monad.Database
import           Schema
import           Stub.Article
import           Stub.User
import           TestEff                 (runTestAppEff')
import           TestMonad
import           TestUtils               (TestAppState, setupInMemoryTests,
                                          setupTests)
import           Types


main :: IO ()
main = do
  -- Setting up test server and config
  -- (sqliteInfo, redisInfo, clientEnv, tid) <- setupTests
  (clientEnv, ref, tid) <- setupInMemoryTests

  -- Running integration tests
  -- runIntegrationTests sqliteInfo redisInfo clientEnv

  -- Running in memory tests with Monad
  runInMemoryTests clientEnv ref

  -- Running in memory tests with Eff
  -- runInMemoryTests' clientEnv ref

  -- Killing test server
  killThread tid

  return ()


runIntegrationTests :: SQLiteInfo -> RedisInfo -> ClientEnv -> IO ()
runIntegrationTests sqliteInfo redisInfo clientEnv = do
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
    $ before (beforeHook4 clientEnv sqliteInfo redisInfo)
    $ after (afterHook4 sqliteInfo redisInfo)
    $ spec4

  hspec
    $ before (beforeHook5 clientEnv sqliteInfo redisInfo)
    $ after (afterHook5 sqliteInfo redisInfo)
    $ spec5

  hspec
    $ before (beforeHook6 clientEnv sqliteInfo redisInfo)
    $ after (afterHook6 sqliteInfo redisInfo)
    $ spec6

runInMemoryTests :: ClientEnv -> MVar TestAppState -> IO ()
runInMemoryTests clientEnv ref = do
  hspec
    $ before (beforeHook1' clientEnv ref)
    $ spec1

  hspec
    $ before (beforeHook2' clientEnv ref)
    $ spec2

  hspec
    $ before (beforeHook3' clientEnv ref)
    $ spec3

  hspec
    $ before (beforeHook4' clientEnv ref)
    $ spec4

-- In Memory Tests for Eff
runInMemoryTests' :: ClientEnv -> MVar TestAppState -> IO ()
runInMemoryTests' clientEnv ref = do
  hspec
    $ before (beforeHook1'' clientEnv ref)
    $ spec1

runAppIgnoreError :: String -> SQLiteInfo -> RedisInfo -> AppMonad a -> IO a
runAppIgnoreError msg sqliteInfo redisInfo action = do
  (result :: Either SomeException a) <- runAppAction sqliteInfo redisInfo action
  case result of
    Left _  -> error msg
    Right r -> return r

beforeHook1'' :: ClientEnv -> MVar TestAppState -> IO (Bool, Bool, Bool)
beforeHook1'' clientEnv ref = do
  callResult <- runClientM (fetchUserClient userid) clientEnv
  env <- liftIO $ readMVar ref
  liftIO $ print env
  runTestAppEff' ref $ do
    inDB       <- isJust <$> EffDatabase.fetchUserDB userid
    inRedis    <- isJust <$> EffCache.fetchCachedUser userid
    let throwsError = isLeft callResult
    return (throwsError, inDB, inRedis)

  where
    userid :: Int64
    userid = 1

beforeHook1' :: ClientEnv -> MVar TestAppState -> IO (Bool, Bool, Bool)
beforeHook1' clientEnv ref = do
  callResult <- runClientM (fetchUserClient userid) clientEnv
  env <- liftIO $ readMVar ref
  liftIO $ print env
  runTestMonad ref $ do
    inDB       <- isJust <$> fetchUserDB userid
    inRedis    <- isJust <$> fetchCachedUser userid
    let throwsError = isLeft callResult
    return (throwsError, inDB, inRedis)

  where
    userid :: Int64
    userid = 1

beforeHook1 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO (Bool, Bool, Bool)
beforeHook1 clientEnv sqliteInfo redisInfo = do
  callResult <- runClientM (fetchUserClient userid) clientEnv
  runAppIgnoreError "beforeHook1 failed" sqliteInfo redisInfo $ do
    inDB       <- isJust <$> fetchUserDB userid
    inRedis    <- isJust <$> fetchCachedUser userid
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
  runAppIgnoreError "beforeHook2 failed" sqliteInfo redisInfo $ do
    case userKeyEither of
      Left _        -> error "DB call failed on spec2"
      Right userKey -> do
        inDB    <- isJust <$> fetchUserDB userKey
        inRedis <- isJust <$> fetchCachedUser userKey
        return (inDB, inRedis, userKey)

  where
    testUser :: User
    testUser = User "Test User" "test.user@test.test" 21 "SWE"

beforeHook2' :: ClientEnv -> MVar TestAppState -> IO (Bool, Bool, Int64)
beforeHook2' clientEnv ref = do
  userKeyEither <- runClientM (createUserClient testUser) clientEnv
  env <- liftIO $ readMVar ref
  liftIO $ (print env)

  runTestMonad ref $ do
    -- env <- liftIO $ readMVar ref
    -- liftIO $ (print env)
    case userKeyEither of
      Left _        -> error "DB call failed on spec2"
      Right userKey -> do
        inDB    <- isJust <$> fetchUserDB userKey
        inRedis <- isJust <$> fetchCachedUser userKey
        return (inDB, inRedis, userKey)

  where
    testUser :: User
    testUser = User "Test User" "test.user@test.test" 21 "SWE"

afterHook :: SQLiteInfo -> RedisInfo -> (Bool, Bool, Int64) -> IO ()
afterHook sqliteInfo redisInfo (_, _, uid) = deleteArtifacts sqliteInfo redisInfo [uid] []

spec2 :: SpecWith (Bool, Bool, Int64)
spec2 = describe "After creating the user but not fetching" $ do
  it "There should be a user in DB" $ \(inDB, _, _) -> inDB `shouldBe` True
  it "There should not be a user in Cache" $ \(_, inRedis, _) -> inRedis `shouldBe` False

beforeHook3 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO (Bool, Bool, Int64)
beforeHook3 clientEnv sqliteInfo redisInfo = do
  userKeyEither <- runClientM (createUserClient testUser1) clientEnv
  case userKeyEither of
    Left _ -> error "DB called failed on spec3"
    Right userKey -> do
      runClientM (fetchUserClient userKey) clientEnv
      runAppIgnoreError "beforeHook3 failed" sqliteInfo redisInfo $ do
        inDB    <- isJust <$> fetchUserDB userKey
        inRedis <- isJust <$> fetchCachedUser userKey
        return (inDB, inRedis, userKey)

beforeHook3' :: ClientEnv -> MVar TestAppState -> IO (Bool, Bool, Int64)
beforeHook3' clientEnv ref = do
  userKeyEither <- runClientM (createUserClient testUser1) clientEnv
  case userKeyEither of
    Left _ -> error "DB called failed on spec3"
    Right userKey -> do
      runClientM (fetchUserClient userKey) clientEnv
      runTestMonad ref $ do
        inDB    <- isJust <$> fetchUserDB userKey
        inRedis <- isJust <$> fetchCachedUser userKey
        return (inDB, inRedis, userKey)

spec3 :: SpecWith (Bool, Bool, Int64)
spec3 = describe "After creating the user and fetching" $ do
  it "There should be a user in DB" $ \(inDB, _, _) -> inDB `shouldBe` True
  it "There should be a user in Cache" $ \(_, inRedis, _) -> inRedis `shouldBe` True

deleteArtifacts ::  SQLiteInfo -> RedisInfo -> [Int64] -> [Int64] -> IO ()
deleteArtifacts sqliteInfo redisInfo userids articleids =
  runAppIgnoreError "Deleting Artifacts" sqliteInfo redisInfo $ do
    forM_ articleids deleteArticleDB
    forM_ userids $ \uid -> do
      deleteCachedUser uid
      deleteUserDB uid

beforeHook4 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO (Bool, Bool, Int64, Int64)
beforeHook4 clientEnv sqliteInfo redisInfo = do
  userKey          <- runSqliteAction sqliteInfo (createUserDB testUser2)
  articleKeyEither <- runClientM (createArticleClient (testArticle1 userKey)) clientEnv
  case articleKeyEither of
    Left e -> error "DB call failed on spec 4!"
    Right articleKey -> do
      fetchResult <- runClientM (fetchArticleClient articleKey) clientEnv
      let callSucceeds = isRight fetchResult
      articleInDB <- isJust <$> runSqliteAction sqliteInfo (fetchArticleDB articleKey)
      return (callSucceeds, articleInDB, userKey, articleKey)

beforeHook4' :: ClientEnv -> MVar TestAppState -> IO (Bool, Bool, Int64, Int64)
beforeHook4' clientEnv ref = do
  userKey          <- runTestMonad ref (createUserDB testUser2)
  articleKeyEither <- runClientM (createArticleClient (testArticle1 userKey)) clientEnv
  case articleKeyEither of
    Left e -> error "DB call failed on spec 4!"
    Right articleKey -> do
      fetchResult <- runClientM (fetchArticleClient articleKey) clientEnv
      let callSucceeds = isRight fetchResult
      articleInDB <- isJust <$> runTestMonad ref (fetchArticleDB articleKey)
      return (callSucceeds, articleInDB, userKey, articleKey)

spec4 :: SpecWith (Bool, Bool, Int64, Int64)
spec4 = describe "After createing a user and an article" $ do
  it "The call should return a result" $ \(succeeds, _, _, _) -> succeeds `shouldBe` True
  it "The article should be in DB" $ \(_, inDB, _, _) -> inDB `shouldBe` True

afterHook4 :: SQLiteInfo -> RedisInfo -> (Bool, Bool, Int64, Int64) -> IO ()
afterHook4 sqliteInfo redisInfo (_, _, userid, articleid) = deleteArtifacts sqliteInfo redisInfo [userid] [articleid]

beforeHook5 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO ([Article], [Article], Int64, Int64, [Int64])
beforeHook5 clientEnv sqliteInfo redisInfo = do
  (uid1, uid2, articleIds) <- runSqliteAction sqliteInfo $ do
    uid1'       <- createUserDB testUser3
    uid2'       <- createUserDB testUser4
    articleIds' <- mapM createArticleDB
      [ testArticle2 uid1', testArticle3 uid1', testArticle4 uid1'
      , testArticle5 uid2', testArticle6 uid2'
      ]
    return (uid1', uid2', articleIds')

  firstArticles  <- runClientM (fetchArticleByAuthorClient uid1) clientEnv
  secondArticles <- runClientM (fetchArticleByAuthorClient uid2) clientEnv

  case (firstArticles, secondArticles) of
    (Right as1, Right as2) -> return (getVal <$> as1, getVal <$> as2, uid1, uid2, articleIds)
    l -> error (show l)

afterHook5 :: SQLiteInfo -> RedisInfo -> ([Article], [Article], Int64, Int64, [Int64]) -> IO ()
afterHook5 sqliteInfo redisInfo (_, _, uid1, uid2, articleIds) = deleteArtifacts sqliteInfo redisInfo [uid1, uid2] articleIds

spec5 :: SpecWith ([Article], [Article], Int64, Int64, [Int64])
spec5 = describe "When fetching articles by author id" $ do
  it "Fetching by the first author should return 3 articles" $ \(firstArticles, _, _, _, _) -> length firstArticles `shouldBe` 3
  it "Fetching by the second author should return 2 articles" $ \(_, secondArticles, _, _, _) -> length secondArticles `shouldBe` 2

beforeHook6 :: ClientEnv -> SQLiteInfo -> RedisInfo -> IO ([(User, Article)], Int64, Int64, [Int64])
beforeHook6 clientEnv sqliteInfo redisInfo = do
  (uid1, uid2, articleIds) <- runSqliteAction sqliteInfo $ do
    uid1' <- createUserDB testUser3
    uid2' <- createUserDB testUser6
    articleIds' <- mapM createArticleDB
      [ testArticle7 uid1', testArticle8 uid1', testArticle9 uid1', testArticle10 uid2'
      , testArticle11 uid2', testArticle12 uid1', testArticle13 uid2', testArticle14 uid2'
      , testArticle15 uid2', testArticle16 uid1', testArticle17 uid1', testArticle18 uid2'
      ]
    return (uid1', uid2', articleIds')

  recentArticles <- runClientM fetchRecentArticlesClient clientEnv
  case recentArticles of
      Right as -> return (entityValTuple <$> as, uid1, uid2, articleIds)
      _        -> error "Spec 6 failed!"

  where
    entityValTuple :: (KeyVal User, KeyVal Article) -> (User, Article)
    entityValTuple (eUser, eArticle) = (getVal eUser, getVal eArticle)

afterHook6 :: SQLiteInfo -> RedisInfo -> ([(User, Article)], Int64, Int64, [Int64]) -> IO ()
afterHook6 sqliteInfo redisInfo (_, uid1, uid2, articleIds) = deleteArtifacts sqliteInfo redisInfo [uid1, uid2] articleIds

spec6 :: SpecWith ([(User, Article)], Int64, Int64, [Int64])
spec6 = describe "When fetching recent articles" $ do
  it "There should be only 10 articles" $ \(xs, _, _, _) -> length xs `shouldBe` 10
