{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Database                    (fetchRedisInfo, fetchSQLiteInfo)

import           Control.Concurrent          (killThread)
import           Data.Either                 (isLeft)
import           Data.Int                    (Int64)
import           Data.Maybe                  (isJust)
import           Database.Persist.Postgresql (fromSqlKey)
import           Servant.Client              (ClientEnv, runClientM)
import           Test.Hspec

import           API                         (createUserClient, fetchUserClient)
import           Database                    (RedisInfo, SQLiteInfo,
                                              deleteUserCache, deleteUserSQLite,
                                              fetchUserRedis, fetchUserSQLite)
import           Schema                      (User (..))
import           TestUtils                   (setupTests)


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
