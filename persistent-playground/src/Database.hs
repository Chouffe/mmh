{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database where

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Logger        (LoggingT, MonadLogger,
                                              runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.Text                   (Text)
import           Database.Esqueleto          (InnerJoin (..), desc, from, limit,
                                              on, orderBy, select, val, where_,
                                              (==.), (^.))
import           Database.Persist            (Entity (..), delete,
                                              get, insert)
import           Database.Persist.Class      (ToBackendKey)
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, withPostgresqlConn)
import           Database.Persist.Sql        (SqlBackend, fromSqlKey,
                                              runSqlConn, toSqlKey)
import           Database.Persist.Sqlite     (runSqlite, withSqliteConn)
import           Data.Int (Int64)

import           Schema
import           Types
import           Monad.Database (MonadDatabase(..))


-- Config
type SQLiteInfo = Text
type PGInfo = ConnectionString

unEntity :: (ToBackendKey SqlBackend a) => Entity a -> KeyVal a
unEntity (Entity id_ val_) = KeyVal (fromSqlKey id_, val_)


instance (MonadIO m, MonadLogger m) => MonadDatabase (SqlPersistT m) where

  fetchUserDB :: Int64 -> SqlPersistT m (Maybe User)
  fetchUserDB uid = get (toSqlKey uid)

  createUserDB :: User -> SqlPersistT m Int64
  createUserDB user = fromSqlKey <$> insert user

  deleteUserDB :: Int64 -> SqlPersistT m ()
  deleteUserDB uid = delete ((toSqlKey uid) :: Key User)

  fetchArticleDB :: Int64 -> SqlPersistT m (Maybe Article)
  fetchArticleDB aid = get (toSqlKey aid)

  createArticleDB :: Article -> SqlPersistT m Int64
  createArticleDB article = fromSqlKey <$> insert article

  deleteArticleDB :: Int64 -> SqlPersistT m ()
  deleteArticleDB aid = delete ((toSqlKey aid) :: Key Article)

  fetchArticlesByAuthorDB :: Int64 -> SqlPersistT m [KeyVal Article]
  fetchArticlesByAuthorDB uid = do
    entities <- select . from $ \articles -> do
      where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
      return articles
    return $ unEntity <$> entities

  fetchRecentArticlesDB :: SqlPersistT m [(KeyVal User, KeyVal Article)]
  fetchRecentArticlesDB = do
    tuples <- select . from $ \(users `InnerJoin` articles) -> do
      on (users ^. UserId ==. articles ^. ArticleAuthorId)
      orderBy [desc (articles ^. ArticlePublishedTime)]
      limit 10
      return (users, articles)
    return $ fmap (\(u, a) -> (unEntity u, unEntity a)) tuples

-- Postgresql Backend

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=password"

fetchPGInfo :: IO PGInfo
fetchPGInfo = return connString

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT
    $ withPostgresqlConn connectionString
    $ \backend -> runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)


-- Sqlite backend

connStringSqlite :: Text
connStringSqlite = "dev.sqlite3"

fetchSQLiteInfo :: ServerMode -> IO SQLiteInfo
fetchSQLiteInfo Test = return "test.sqlite3"
fetchSQLiteInfo Dev  = return "dev.sqlite3"
fetchSQLiteInfo Prod = return "prod.sqlite3"

migrateSqliteDB :: IO ()
migrateSqliteDB = runSqlite connStringSqlite (runMigration migrateAll)

runSqliteAction :: Text -> SqlPersistT (LoggingT IO) a -> IO a
runSqliteAction connectionString query =
  runStdoutLoggingT
    $ withSqliteConn connectionString
    $ \backend -> runSqlConn query backend
