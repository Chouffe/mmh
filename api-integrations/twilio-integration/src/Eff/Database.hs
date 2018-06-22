{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Eff.Database
  ( fetchConfig
  , migrateDB
  , Config (..)

  , Database (..)
  , registerUser
  , retrieveSubscribers
  , runDatabase
  )
  where

import           Control.Monad                (void)
import           Control.Monad.Freer.Extended (Eff, Member, send, runNat)
import           Control.Monad.Logger         (LoggingT, runStdoutLoggingT)
import           Control.Monad.Reader         (runReaderT)
-- import           Data.ByteString.Char8        (pack)
import           Data.ByteString              (ByteString)
import           Data.Text                    (Text)
import           Database.Persist             (insert, selectList)
import           Database.Persist.Sql         (SqlPersistT, entityVal)
import           Database.Persist.Postgresql  (withPostgresqlConn, runMigration)
-- import           System.Environment           (getEnv)

import           Schema

data Config
  = Config
    { cConnection :: ByteString
    }
  deriving (Eq, Show)

data Database a where
  RegisterUser        :: Text -> Database ()
  RetrieveSubscribers :: Database [Text]

registerUser :: (Member Database r) => Text -> Eff r ()
registerUser = send . RegisterUser

retrieveSubscribers :: (Member Database r) => Eff r [Text]
retrieveSubscribers = send RetrieveSubscribers

runPGAction :: Config -> SqlPersistT (LoggingT IO) a -> IO a
runPGAction config action =
  runStdoutLoggingT
    $ withPostgresqlConn (cConnection config)
    $ \backend -> runReaderT action backend

runDatabase :: (Member IO r) => Config -> Eff (Database ': r) a -> Eff r a
runDatabase config = runNat databaseToIO

  where
    databaseToIO :: Database a -> IO a
    databaseToIO (RegisterUser email) =
      runPGAction config $ void $ insert (Subscriber email)

    databaseToIO RetrieveSubscribers = do
        allEntities <- runPGAction config (selectList [] [])
        return $ (subscriberEmail . entityVal) <$> allEntities

fetchConfig :: IO Config
fetchConfig = do
  -- pgConn <- pack <$> getEnv "DATABASE_URL"
  return $ Config defaultPGConn
  where
    defaultPGConn :: ByteString
    defaultPGConn = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=password"

migrateDB :: Config -> IO ()
migrateDB config = runPGAction config $ runMigration migrateAll
