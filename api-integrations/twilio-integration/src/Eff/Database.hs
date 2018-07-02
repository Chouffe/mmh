{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Eff.Database
  ( fetchConfig
  , migrateDB
  , Config (..)

  , Handle (..)
  , withHandle

  , Database (..)
  , registerUser
  , retrieveSubscribers
  , runDatabase
  )
  where

import           Control.Monad                (void)
import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Freer.Extended (Eff, Member, send, runNat)
import           Control.Monad.Logger         (LoggingT, runStdoutLoggingT, runNoLoggingT)
import           Control.Monad.Reader         (runReaderT)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as BS
import           Data.Monoid                  ((<>))
import           Data.Pool                    as Pool
import           Data.Text                    (Text)
import           Database.Persist             (insert, selectList)
import           Database.Persist.Sql         (SqlPersistT, entityVal, ConnectionPool, runSqlPool)
import           Database.Persist.Postgresql  (withPostgresqlConn, runMigration, createPostgresqlPool, ConnectionString)

import           Schema
import           Types                        (Environment (..))

data Config
  = Config
    { cConnection :: ByteString
    }
  deriving (Eq, Show)

data Handle
  = Handle
    { hConfig :: Config
    , hPool   :: ConnectionPool
    }

connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=127.0.0.1 port=5432 user=postgres dbname=postgres" <> sfx <> " password=password"

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

-- setLogger :: Environment -> Middleware
-- setLogger Development = logStdoutDev
-- setLogger Test        = id
-- setLogger Production  = logStdout


makePool :: Environment -> IO ConnectionPool
makePool Development = runStdoutLoggingT (createPostgresqlPool (connStr "") (envPool Development))
makePool Test        = runNoLoggingT (createPostgresqlPool (connStr "") (envPool Test))
makePool Production  = runStdoutLoggingT (createPostgresqlPool (connStr "") (envPool Production))

-- TODO: use bracket instead to shield from exceptions
withHandle :: Environment -> Config -> (Handle -> IO a) -> IO a
withHandle env config f = do
  pool <- liftIO $ makePool env
  let handle = Handle config pool
  x <- f handle
  return x

data Database a where
  RegisterUser        :: Text -> Database ()
  RetrieveSubscribers :: Database [Text]

registerUser :: (Member Database r) => Text -> Eff r ()
registerUser = send . RegisterUser

retrieveSubscribers :: (Member Database r) => Eff r [Text]
retrieveSubscribers = send RetrieveSubscribers

runPGAction :: Handle -> SqlPersistT IO a -> IO a
runPGAction Handle{..} action =
  runSqlPool action hPool
  -- runStdoutLoggingT
  --   $ withPostgresqlConn (cConnection config)
  --   $ \backend -> runReaderT action backend

runDatabase :: (Member IO r) => Handle -> Eff (Database ': r) a -> Eff r a
runDatabase handle = runNat databaseToIO

  where
    databaseToIO :: Database a -> IO a
    databaseToIO (RegisterUser email) =
      runPGAction handle $ void $ insert (Subscriber email)

    databaseToIO RetrieveSubscribers = do
        allEntities <- runPGAction handle (selectList [] [])
        return $ (subscriberEmail . entityVal) <$> allEntities

fetchConfig :: IO Config
fetchConfig = do
  -- pgConn <- pack <$> getEnv "DATABASE_URL"
  return $ Config defaultPGConn

  where
    defaultPGConn :: ByteString
    defaultPGConn = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=password"

migrateDB :: Handle -> IO ()
migrateDB handle = runPGAction handle $ runMigration migrateAll
