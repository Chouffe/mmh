{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import           Control.Concurrent      (ThreadId, forkIO, threadDelay)
import           Control.Monad.Logger    (runStdoutLoggingT)
import           Database.Persist.Sqlite (runMigration, runSqlite)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv (..), parseBaseUrl)

import           API                     (fetchConfig, port, redisInfo,
                                          runServer, sqliteInfo)
import           Database                (SQLiteInfo)

import           Cache
import           Schema
import           Types


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
