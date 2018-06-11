{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module SQLite where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Conduit            (($$))
import           Data.Conduit.List       as CL
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Database.Persist        (Entity, SelectOpt (..), delete, get,
                                          insert, selectList, (<.), (==.))
import           Database.Persist.Sql    (SqlPersistT, rawQuery, runMigration,
                                          runSqlConn, toSqlKey)
import           Database.Persist.Sqlite (runSqlite, withSqliteConn)

import           Schema


connStringSqlite :: Text
connStringSqlite = "dev.sqlite3"

migrateSqliteDB :: IO ()
migrateSqliteDB = runSqlite connStringSqlite (runMigration migrateAll)

runSqliteAction :: Text -> SqlPersistT (LoggingT IO) a -> IO a
runSqliteAction connectionString query =
  runStdoutLoggingT
    $ withSqliteConn connectionString
    $ \backend -> runSqlConn query backend

action1 :: IO ()
action1 = runSqliteAction connStringSqlite $ do
  michelId <- insert $ User "Michel Michel" "michel.michel@gmail.com" 32 "Astronaut"
  liftIO $ print michelId
  michel <- get michelId
  liftIO $ print michel
  return ()

action2 :: Integer -> IO ()
action2 userId = runSqliteAction connStringSqlite $ do
  (muser :: Maybe User) <- get userIdKey
  liftIO $ print muser
  case muser of
    Nothing -> liftIO $ putStrLn "User Id is dead..."
    Just _  -> do
      delete userIdKey
      liftIO $ putStrLn "User id is now deleted"
  where
    userIdKey = toSqlKey (fromIntegral userId)

action3 :: IO ()
action3 = runSqliteAction connStringSqlite $ do
  liftIO $ putStrLn "DUMPING TABLE"
  -- dumpTable
  -- return ()

 -- where
   -- dumpTable = rawQuery "SELECT * FROM users" [] $$ CL.mapM_ (liftIO . print)
