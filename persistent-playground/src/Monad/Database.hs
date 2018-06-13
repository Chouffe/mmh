module Monad.Database where

import           Data.Int (Int64)
import           Schema
import           Types


class (Monad m) => MonadDatabase m where
  fetchUserDB             :: Int64 -> m (Maybe User)
  createUserDB            :: User -> m Int64
  deleteUserDB            :: Int64 -> m ()
  fetchArticleDB          :: Int64 -> m (Maybe Article)
  createArticleDB         :: Article -> m Int64
  deleteArticleDB         :: Int64 -> m ()
  fetchArticlesByAuthorDB :: Int64 -> m [KeyVal Article]
  fetchRecentArticlesDB   :: m [(KeyVal User, KeyVal Article)]
