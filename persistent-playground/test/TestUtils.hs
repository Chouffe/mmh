{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestUtils where

import           Control.Concurrent      (ThreadId, forkIO, threadDelay)
import           Control.Monad.Logger    (runStdoutLoggingT)
import           Control.Monad.Reader    (runReaderT)
import           Database.Persist.Sqlite (runMigration, runMigrationSilent,
                                          runSqlite)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv (..), parseBaseUrl)

import           API                     (fetchConfig, port, redisInfo,
                                          runServer, sqliteInfo)
import           Database                (SQLiteInfo, insertUser,
                                          migrateSqliteDB, runAction)

import           Schema
import           Types


-- TODO: fixme
setupTests :: IO (SQLiteInfo, RedisInfo, ClientEnv, ThreadId)
setupTests = do
  config     <- fetchConfig Test
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "http://127.0.0.1:" ++ show (port config)

  let clientEnv = ClientEnv mgr baseUrl
  -- let connString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=password"

  -- Silent DB Migration
  runStdoutLoggingT
    $ runSqlite (sqliteInfo config)
    $ runMigration migrateAll

  -- Insert a row in DB
  -- runAction connString (runMigration migrateAll)
  -- runAction connString (insertUser meUser)

  tid <- forkIO $ runServer Test
  threadDelay 1000000
  return (sqliteInfo config, redisInfo config, clientEnv, tid)

  -- where
  --   meUser = User "john Doe" "john.doe@gmail.com" 39 "SWE"
