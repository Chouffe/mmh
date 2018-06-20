{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Eff.Database where

import           Database.Esqueleto          (InnerJoin (..), desc, from, limit,
                                              on, orderBy, select, val, where_,
                                              (==.), (^.))
import           Database.Persist            (delete, get, insert)
import           Database.Persist.Sql        (SqlPersistT, fromSqlKey, toSqlKey)
import           Control.Monad.Logger        (LoggingT)
import           Control.Monad.Freer         (Eff, Member, send)


import           Data.Int                    (Int64)
import           Schema
import           Eff.Utils                   (runNat)
import           Database
import           Types                       (KeyVal)


data Database a where
  FetchUserDB             :: Int64 -> Database (Maybe User)
  CreateUserDB            :: User -> Database Int64
  DeleteUserDB            :: Int64 -> Database ()
  AllUsersDB              :: Database [KeyVal User]
  FetchArticleDB         :: Int64 -> Database (Maybe Article)
  CreateArticleDB         :: Article -> Database Int64
  DeleteArticleDB         :: Int64 -> Database ()
  FetchArticlesByAuthorDB :: Int64 -> Database [(KeyVal Article)]
  FetchRecentArticlesDB   :: Database [(KeyVal User, KeyVal Article)]

-- Boilerplate: can use TH to automate the boilerplate
fetchUserDB :: (Member Database r) => Int64 -> Eff r (Maybe User)
fetchUserDB uid = send $ FetchUserDB uid

createUserDB :: (Member Database r) => User -> Eff r Int64
createUserDB user = send $ CreateUserDB user

deleteUserDB :: (Member Database r) => Int64 -> Eff r ()
deleteUserDB uid = send $ DeleteUserDB uid

allUsersDB :: (Member Database r) => Eff r [KeyVal User]
allUsersDB = send AllUsersDB

fetchArticleDB :: (Member Database r) => Int64 -> Eff r (Maybe Article)
fetchArticleDB = send . FetchArticleDB

createArticleDB :: (Member Database r) => Article -> Eff r Int64
createArticleDB = send . CreateArticleDB

deleteArticleDB :: (Member Database r) => Int64 -> Eff r ()
deleteArticleDB = send . DeleteArticleDB

fetchArticlesByAuthorDB :: (Member Database r) => Int64 -> Eff r [(KeyVal Article)]
fetchArticlesByAuthorDB = send . FetchArticlesByAuthorDB

fetchRecentArticlesDB :: (Member Database r) => Eff r [(KeyVal User, KeyVal Article)]
fetchRecentArticlesDB = send FetchRecentArticlesDB

-- Interpreter
runDatabase :: (Member (SqlPersistT (LoggingT IO)) r) => Eff (Database ': r) a -> Eff r a
runDatabase = runNat databaseToSql
  where
    -- Natural transformation from Database to SqlPersistT (LoggingT IO)
    databaseToSql :: Database a -> SqlPersistT (LoggingT IO) a
    databaseToSql (FetchUserDB uid) = get (toSqlKey uid)
    databaseToSql (CreateUserDB user) = fromSqlKey <$> insert user
    databaseToSql (DeleteUserDB uid) = delete ((toSqlKey uid) :: Key User)
    databaseToSql AllUsersDB = do
      entities <- select . from $ \users -> do
        limit 100
        return users
      return $ unEntity <$> entities
    databaseToSql (FetchArticleDB aid) = get (toSqlKey aid)
    databaseToSql (CreateArticleDB article) = fromSqlKey <$> insert article
    databaseToSql (DeleteArticleDB aid) = delete ((toSqlKey aid) :: Key Article)
    databaseToSql FetchRecentArticlesDB = do
      tuples <- select . from $ \(users `InnerJoin` articles) -> do
        on (users ^. UserId ==. articles ^. ArticleAuthorId)
        orderBy [desc (articles ^. ArticlePublishedTime)]
        limit 10
        return (users, articles)
      return $ fmap (\(u, a) -> (unEntity u, unEntity a)) tuples
    databaseToSql (FetchArticlesByAuthorDB uid) = do
      entities <- select . from $ \articles -> do
        where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
        return articles
      return $ unEntity <$> entities


runSqlPersist :: (Member IO r) => SQLiteInfo -> Eff (SqlPersistT (LoggingT IO) ': r) a -> Eff r a
runSqlPersist sqliteInfo = runNat sqlToIO
  where
    sqlToIO :: SqlPersistT (LoggingT IO) a -> IO a
    sqlToIO action = runSqliteAction sqliteInfo action
