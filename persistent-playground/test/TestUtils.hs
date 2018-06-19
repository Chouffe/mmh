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

import           API                      (fetchConfig, port, redisInfo,
                                           runServer, sqliteInfo, usersAPI)
import           Database                 (SQLiteInfo)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server

import           API
import           Cache
import           Schema
import           TestMonad
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
    $ serve usersAPI
    $ testAPIServer
    $ transformTestToHandler ref

  threadDelay 1000000
  return (clientEnv, ref, tid)


testAPIServer :: (TestMonad :~> Handler) -> Server UsersAPI
testAPIServer nt =
  enter nt
    $ fetchUserHandler
    :<|> createUserHandler
    :<|> allUsersHandler
