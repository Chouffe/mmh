{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module TestUtils where

import           Control.Concurrent       (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.MVar  (MVar, newMVar)
import           Control.Monad.Logger     (runStdoutLoggingT)
import qualified Data.Map                 as Map
import           Database.Persist.Sqlite  (runMigration, runSqlite)
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Servant.Client           (ClientEnv (..), parseBaseUrl)

import           API                      (fetchConfig, fullAPI, port,
                                           redisInfo, runServer, sqliteInfo)
import           Database                 (SQLiteInfo)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server

import           API
import qualified APIEff
import           Cache
import           Schema
import           TestEff                  (TestAppEff (..))
import qualified TestEff                  as TE
import           TestMonad                (ArticleMap, TestMonad, UserMap)
import qualified TestMonad                as TM
import           Types


type TestAppState = (UserMap, ArticleMap, UserMap)

setupTests :: IO (SQLiteInfo, RedisInfo, ClientEnv, ThreadId)
setupTests = do
  config     <- fetchConfig Test
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "http://127.0.0.1:" ++ show (port config)

  let clientEnv = ClientEnv mgr baseUrl

  runStdoutLoggingT
    $ runSqlite (sqliteInfo config)
    $ runMigration migrateAll

  tid <- forkIO $ runServer Test
  threadDelay 1000000
  return (sqliteInfo config, redisInfo config, clientEnv, tid)

setupInMemoryTests :: IO (ClientEnv, MVar TestAppState, ThreadId)
setupInMemoryTests = do
  config     <- fetchConfig Test
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "http://127.0.0.1:" ++ show (port config)

  let clientEnv = ClientEnv mgr baseUrl
  let initialMap = (Map.empty, Map.empty, Map.empty) :: TestAppState

  ref <- newMVar initialMap
  putStrLn $ "Starting test server on port: " ++ show (port config)
  tid <- forkIO
    $ run (port config)
    $ serve fullAPI
    $ testAPIFullServer
    $ TM.transformTestToHandler ref

  threadDelay 1000000
  return (clientEnv, ref, tid)

-- In memory tests with Eff instead of AppMonad
setupInMemoryTests' :: IO (ClientEnv, MVar TestAppState, ThreadId)
setupInMemoryTests' = do
  config     <- fetchConfig Test
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "http://127.0.0.1:" ++ show (port config)

  let clientEnv = ClientEnv mgr baseUrl
  let initialMap = (Map.empty, Map.empty, Map.empty) :: TestAppState

  ref <- newMVar initialMap
  putStrLn $ "Starting test server on port: " ++ show (port config)
  tid <- forkIO
    $ run (port config)
    $ serve fullAPI
    $ testAPIFullServer'
    $ TE.transformTestEffToHandler ref

  threadDelay 1000000
  return (clientEnv, ref, tid)

-- Test API and Server

-- For TestMonad
testAPIServer :: (TestMonad :~> Handler) -> Server UsersAPI
testAPIServer nt =
  enter nt
    $ fetchUserHandler
    :<|> createUserHandler
    :<|> allUsersHandler

testUsersAPIServer :: (TestMonad :~> Handler) -> Server UsersAPI
testUsersAPIServer nt =
  enter nt
  $ fetchUserHandler
  :<|> createUserHandler
  :<|> allUsersHandler

testArticlesAPIServer :: (TestMonad :~> Handler) -> Server ArticlesAPI
testArticlesAPIServer nt =
  enter nt
  $ fetchArticleHandler
  :<|> createArticleHandler
  :<|> fetchArticleByAuthorHandler
  :<|> fetchRecentArticlesHandler

testAPIFullServer :: (TestMonad :~> Handler) -> Server FullAPI
testAPIFullServer nt = testUsersAPIServer nt :<|> testArticlesAPIServer nt

-- For Eff Monad
testAPIServer' :: (TestAppEff :~> Handler) -> Server UsersAPI
testAPIServer' nt =
  enter nt
    $ APIEff.fetchUserHandler
    :<|> APIEff.createUserHandler
    :<|> APIEff.allUsersHandler

testUsersAPIServer' :: (TestAppEff :~> Handler) -> Server UsersAPI
testUsersAPIServer' nt =
  enter nt
  $ APIEff.fetchUserHandler
  :<|> APIEff.createUserHandler
  :<|> APIEff.allUsersHandler

testArticlesAPIServer' :: (TestAppEff :~> Handler) -> Server ArticlesAPI
testArticlesAPIServer' nt =
  enter nt
  $ APIEff.fetchArticleHandler
  :<|> APIEff.createArticleHandler
  :<|> APIEff.fetchArticleByAuthorHandler
  :<|> APIEff.fetchRecentArticlesHandler

testAPIFullServer' :: (TestAppEff :~> Handler) -> Server FullAPI
testAPIFullServer' nt = testUsersAPIServer' nt :<|> testArticlesAPIServer' nt
