{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module TestMonad where


import           Control.Concurrent.MVar    (MVar, readMVar, swapMVar)
import           Control.Exception          (Exception, SomeException, handle)
import           Control.Lens               (view, _1, _2, _3)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State        (StateT, get, put, runStateT)
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Int                   (Int64)
import qualified Data.List                  as List
import qualified Data.Map                   as Map
import           Database.Persist.Sql       (fromSqlKey)
import           Servant                    (throwError)
import           Servant.Server

import           Monad.Cache
import           Monad.Database
import           Schema
import           Types


type UserMap = Map.Map Int64 User
type ArticleMap = Map.Map Int64 Article

newtype TestMonad a = TestMonad (StateT (UserMap, ArticleMap, UserMap) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadCache TestMonad where
  cacheUser        :: Int64 -> User -> TestMonad ()
  cacheUser uid user = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newUserCache = Map.insert uid user userCache
    put (userDB, articleDB, newUserCache)

  fetchCachedUser  :: Int64 -> TestMonad (Maybe User)
  fetchCachedUser uid = TestMonad $ do
    userCache <- (view _3) <$> get
    return $ Map.lookup uid userCache

  deleteCachedUser :: Int64 -> TestMonad ()
  deleteCachedUser uid = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newUserCache = Map.delete uid userCache
    put (userDB, articleDB, newUserCache)


instance MonadDatabase TestMonad where
  fetchUserDB :: Int64 -> TestMonad (Maybe User)
  fetchUserDB uid = TestMonad $ do
    userDB <- (view _1) <$> get
    return $ Map.lookup uid userDB

  createUserDB :: User -> TestMonad Int64
  createUserDB user = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newUid = if Map.null userDB
                 then 1
                 else 1 + (fst . Map.findMax) userDB
    let newUserDB = Map.insert newUid user userDB
    put (newUserDB, articleDB, userCache)
    return newUid

  deleteUserDB :: Int64 -> TestMonad ()
  deleteUserDB uid = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newUserDB = Map.delete uid userDB
    put (newUserDB, articleDB, userCache)

  allUsersDB :: TestMonad [KeyVal User]
  allUsersDB = TestMonad $ do
    userDB <- (view _1) <$> get
    return $ KeyVal <$> Map.toList userDB

  fetchArticleDB :: Int64 -> TestMonad (Maybe Article)
  fetchArticleDB aid = TestMonad $ do
    articleDB <- (view _2) <$> get
    return $ Map.lookup aid articleDB

  createArticleDB :: Article -> TestMonad Int64
  createArticleDB article = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newUid = if Map.null userDB
                 then 1
                 else 1 + (fst . Map.findMax) userDB
    let newArticleDB = Map.insert newUid article articleDB
    put (userDB, newArticleDB, userCache)
    return newUid

  deleteArticleDB :: Int64 -> TestMonad ()
  deleteArticleDB aid = TestMonad $ do
    (userDB, articleDB, userCache) <- get
    let newArticleDB = Map.delete aid articleDB
    put (userDB, newArticleDB, userCache)

  fetchArticlesByAuthorDB :: Int64 -> TestMonad [KeyVal Article]
  fetchArticlesByAuthorDB uid = TestMonad $ do
    articleDB <- (view _2) <$> get
    return
      $ map KeyVal
      $ Map.toList
      $ Map.filter (\article -> fromSqlKey (articleAuthorId article) == uid)
      $ articleDB


  fetchRecentArticlesDB :: TestMonad [(KeyVal User, KeyVal Article)]
  fetchRecentArticlesDB = TestMonad $ do
    (userDB, articleDB, _) <- get
    let recentArticles = List.take 10 (List.sortBy orderByTimestamp (Map.toList articleDB))
    return $ map (matchWithAuthor userDB) recentArticles

    where
      orderByTimestamp :: (a, Article) -> (a1, Article) -> Ordering
      orderByTimestamp (_, article1) (_, article2) =
        articlePublishedTime article2 `compare` articlePublishedTime article1

      matchWithAuthor :: Map.Map Int64 a -> (Int64, Article) -> (KeyVal a, KeyVal Article)
      matchWithAuthor userDB (aid, article) =
        case Map.lookup (fromSqlKey (articleAuthorId article)) userDB of
          Nothing -> error "Found article with no user"
          Just u -> (KeyVal (fromSqlKey (articleAuthorId article), u), KeyVal (aid, article))


runStateTWithPointer :: (Exception e, MonadIO m) => StateT s m a -> MVar s -> m (Either e a)
runStateTWithPointer action ref = do
  env <- liftIO $ readMVar ref
  (val, newEnv) <- runStateT action env
  liftIO $ swapMVar ref newEnv
  return $ Right val

runTestMonad :: MVar (UserMap, ArticleMap, UserMap) -> TestMonad a -> IO a
runTestMonad ref (TestMonad action) = do
  currentState <- readMVar ref
  (result, newRef) <- runStateT action currentState
  swapMVar ref newRef
  return result

transformTestToHandler :: MVar (UserMap, ArticleMap, UserMap) -> TestMonad :~> Handler
transformTestToHandler ref = NT $ \(TestMonad action) -> do
  result <- liftIO
    $ handle handler
    $ runStateTWithPointer action ref
  Handler $ either throwError return result
  where
    handler :: SomeException -> IO (Either ServantErr a)
    handler e = return $ Left err500 { errBody = pack (show e) }
