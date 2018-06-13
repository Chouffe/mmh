{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module Monad.App where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Data.Int                   (Int64)
import           Database.Persist.Sql       (SqlPersistT)
import           Database.Redis             (Redis, connect, runRedis)

import           Cache
import           Database
import           Monad.Cache
import           Monad.Database
import           Schema
import           Types


newtype AppMonad a
  = AppMonad (ReaderT RedisInfo (SqlPersistT (LoggingT IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

liftSqlPersistT :: SqlPersistT (LoggingT IO) a -> AppMonad a
liftSqlPersistT action = AppMonad $ ReaderT (const action)

liftRedis :: Redis a -> AppMonad a
liftRedis action = do
  info       <- AppMonad ask
  connection <- liftIO $ connect info
  liftIO $ runRedis connection action


instance MonadCache AppMonad where
  cacheUser :: Int64 -> User -> AppMonad ()
  cacheUser uid user = liftRedis (cacheUser uid user)

  fetchCachedUser :: Int64 -> AppMonad (Maybe User)
  fetchCachedUser = liftRedis . fetchCachedUser

  deleteCachedUser :: Int64 -> AppMonad ()
  deleteCachedUser = liftRedis . deleteCachedUser


instance MonadDatabase AppMonad where
  fetchUserDB             :: Int64 -> AppMonad (Maybe User)
  fetchUserDB = liftSqlPersistT . fetchUserDB

  createUserDB            :: User -> AppMonad Int64
  createUserDB = liftSqlPersistT . createUserDB

  deleteUserDB            :: Int64 -> AppMonad ()
  deleteUserDB = liftSqlPersistT . deleteUserDB

  fetchArticleDB          :: Int64 -> AppMonad (Maybe Article)
  fetchArticleDB = liftSqlPersistT . fetchArticleDB

  createArticleDB         :: Article -> AppMonad Int64
  createArticleDB = liftSqlPersistT . createArticleDB

  deleteArticleDB         :: Int64 -> AppMonad ()
  deleteArticleDB = liftSqlPersistT . deleteArticleDB

  fetchArticlesByAuthorDB :: Int64 -> AppMonad [KeyVal Article]
  fetchArticlesByAuthorDB = liftSqlPersistT . fetchArticlesByAuthorDB

  fetchRecentArticlesDB   :: AppMonad [(KeyVal User, KeyVal Article)]
  fetchRecentArticlesDB = liftSqlPersistT fetchRecentArticlesDB
