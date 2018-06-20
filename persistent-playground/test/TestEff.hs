{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestEff
  ( transformTestEffToHandler
  , runTestAppEff
  , runTestAppEff'
  , runTestDatabase
  , runTestCache
  , TestAppEff (..)
  )
  where

import           Control.Concurrent.MVar    (MVar, readMVar, swapMVar)
import           Control.Exception          (Exception, SomeException, handle)
import           Control.Lens               (view, _1, _2, _3)
import           Control.Monad.Freer        (Eff, Member, runM)
import           Control.Monad.State        (StateT, put, get, runStateT)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Int                   (Int64)
import           Database.Persist.Sql       (fromSqlKey)
import           Servant                    (throwError)
import           Servant.Server             ((:~>) (..), Handler (..))
import           Servant.Server             (ServantErr (..), err500)

import           Eff.Cache
import           Eff.Database
import           Eff.Utils                  (runNat)
import           Schema
import           TestMonad                  (runStateTWithPointer)
import           TestMonad                  (ArticleMap, UserMap)
import           Types                      (KeyVal (..))


type TestAppState = (UserMap, ArticleMap, UserMap)
type TestAppEff  = Eff '[Cache, Database, StateT TestAppState IO]

runTestCache :: (Member (StateT TestAppState IO)) r
             => Eff (Cache ': r) a
             -> Eff r a
runTestCache = runNat cacheToState
  where
    cacheToState :: Cache a -> StateT TestAppState IO a
    cacheToState (CacheUser uid user) = do
      (userDB, articleDB, userCache) <- get
      let newUserCache = Map.insert uid user userCache
      put (userDB, articleDB, newUserCache)

    cacheToState (FetchCachedUser uid) = do
      userCache <- (view _3) <$> get
      return $ Map.lookup uid userCache

    cacheToState (DeleteCachedUser uid) = do
      (userDB, articleDB, userCache) <- get
      let newUserCache = Map.delete uid userCache
      put (userDB, articleDB, newUserCache)


runTestDatabase :: (Member (StateT TestAppState IO)) r
                => Eff (Database ': r) a
                -> Eff r a
runTestDatabase = runNat databaseToState
  where
    databaseToState :: Database a -> StateT TestAppState IO a
    databaseToState (FetchUserDB uid) = do
      userDB <- (view _1) <$> get
      return $ Map.lookup uid userDB

    databaseToState (CreateUserDB user) = do
      (userDB, articleDB, userCache) <- get
      let newUid = if Map.null userDB
                   then 1
                   else 1 + (fst . Map.findMax) userDB
      let newUserDB = Map.insert newUid user userDB
      put (newUserDB, articleDB, userCache)
      return newUid

    databaseToState (DeleteUserDB uid) = do
      (userDB, articleDB, userCache) <- get
      let newUserDB = Map.delete uid userDB
      put (newUserDB, articleDB, userCache)

    databaseToState AllUsersDB = do
      userDB <- (view _1) <$> get
      return $ KeyVal <$> Map.toList userDB

    databaseToState (FetchArticleDB aid) = do
      articleDB <- (view _2) <$> get
      return $ Map.lookup aid articleDB

    databaseToState (CreateArticleDB article) = do
      (userDB, articleDB, userCache) <- get
      let newUid = if Map.null userDB
                   then 1
                   else 1 + (fst . Map.findMax) userDB
      let newArticleDB = Map.insert newUid article articleDB
      put (userDB, newArticleDB, userCache)
      return newUid

    databaseToState (DeleteArticleDB aid) = do
      (userDB, articleDB, userCache) <- get
      let newArticleDB = Map.delete aid articleDB
      put (userDB, newArticleDB, userCache)

    databaseToState (FetchArticlesByAuthorDB uid) = do
      articleDB <- (view _2) <$> get
      return
        $ map KeyVal
        $ Map.toList
        $ Map.filter (\article -> fromSqlKey (articleAuthorId article) == uid)
        $ articleDB

    databaseToState FetchRecentArticlesDB =
      let
        orderByTimestamp :: (a, Article) -> (a1, Article) -> Ordering
        orderByTimestamp (_, article1) (_, article2) =
          articlePublishedTime article2 `compare` articlePublishedTime article1

        matchWithAuthor :: Map.Map Int64 a -> (Int64, Article) -> (KeyVal a, KeyVal Article)
        matchWithAuthor userDB (aid, article) =
          case Map.lookup (fromSqlKey (articleAuthorId article)) userDB of
            Nothing -> error "Found article with no user"
            Just u -> (KeyVal (fromSqlKey (articleAuthorId article), u), KeyVal (aid, article))

      in do
        (userDB, articleDB, _) <- get
        let recentArticles = List.take 10 (List.sortBy orderByTimestamp (Map.toList articleDB))
        return $ map (matchWithAuthor userDB) recentArticles

runTestAppEff' :: MVar TestAppState -> TestAppEff a -> IO a
runTestAppEff' ref eff = do
  currentState <- readMVar ref
  let stateAction = runM $ runTestDatabase $ runTestCache eff
  (result, newRef) <- runStateT stateAction currentState
  swapMVar ref newRef
  return result

runTestAppEff :: Exception e => MVar TestAppState -> TestAppEff a -> IO (Either e a)
runTestAppEff ref eff = do
  let stateAction = runM $ runTestDatabase $ runTestCache eff
  runStateTWithPointer stateAction ref

transformTestEffToHandler :: MVar TestAppState -> TestAppEff :~> Handler
transformTestEffToHandler ref = NT $ \eff -> do
  result <- liftIO
    $ handle handler
    $ runTestAppEff ref eff
  Handler $ either throwError return result

  where
    handler :: SomeException -> IO (Either ServantErr a)
    handler e = return $ Left err500 { errBody = pack (show e) }
