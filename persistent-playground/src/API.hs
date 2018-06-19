{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module API where

import           Data.Int                 (Int64)
import           Data.Proxy               (Proxy (..))
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client           (ClientM, client)
import           Servant.Server

import           Cache
import           Database
import           Monad.App
import           Monad.Cache
import           Monad.Database
import           Schema
import           Types

data Config
  = Config { port       :: Int
           , redisInfo  :: RedisInfo
           , sqliteInfo :: SQLiteInfo
           }
  deriving Show


-- API type and Server

-- Users API
type UsersAPI
  = "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "users" :> "all" :> Get '[JSON] [KeyVal User]

-- Articles API
type ArticlesAPI
  = "articles" :> Capture "articleid" Int64 :> Get '[JSON] Article
  :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Int64
  :<|> "articles" :> "author" :> Capture "authorid" Int64 :> Get '[JSON] [KeyVal Article]
  :<|> "articles" :> "recent" :> Get '[JSON] [(KeyVal User, KeyVal Article)]

type FullAPI
  = UsersAPI
  :<|> ArticlesAPI

usersServer :: (AppMonad :~> Handler) -> Server UsersAPI
usersServer naturalTransformation =
  enter naturalTransformation
    $ fetchUserHandler
    :<|> createUserHandler
    :<|> allUsersHandler

articlesServer :: (AppMonad :~> Handler) -> Server ArticlesAPI
articlesServer naturalTransformation =
  enter naturalTransformation
  $ fetchArticleHandler
  :<|> createArticleHandler
  :<|> fetchArticleByAuthorHandler
  :<|> fetchRecentArticlesHandler

fullServer :: (AppMonad :~> Handler) -> Server FullAPI
fullServer naturalTransformation =
       usersServer naturalTransformation
  :<|> articlesServer naturalTransformation


usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

fullAPI :: Proxy FullAPI
fullAPI = Proxy :: Proxy FullAPI

getPort :: ServerMode -> Int
getPort Prod = 8000
getPort Dev  = 8001
getPort Test = 8002

fetchConfig :: ServerMode -> IO Config
fetchConfig serverMode = do
  sqlInfo  <- fetchSQLiteInfo serverMode
  rInfo    <- fetchRedisInfo serverMode
  let prt = getPort serverMode
  return $ Config prt rInfo sqlInfo

runServer :: ServerMode -> IO ()
runServer serverMode = do
  putStrLn $ "Fetching Configuration for: " ++ show serverMode
  config <- fetchConfig serverMode
  print config

  putStrLn $ "Running server on port: " ++ (show (port config))
  run (port config)
    $ serve fullAPI
    $ fullServer
    $ transformAppToHandler (sqliteInfo config) (redisInfo config)

fetchUserHandler :: (MonadDatabase m, MonadCache m) => Int64 -> m User
fetchUserHandler uid = do
  muser <- fetchCachedUser uid
  case muser of
    Just user -> return user
    Nothing   -> do
      muser' <- fetchUserDB uid
      case muser' of
        Just user -> do
          cacheUser uid user
          return user
        Nothing   -> error "Could not find user with ID"

createUserHandler :: MonadDatabase m => User -> m Int64
createUserHandler = createUserDB

allUsersHandler :: MonadDatabase m => m [KeyVal User]
allUsersHandler = allUsersDB

fetchArticleHandler :: MonadDatabase m => Int64 -> m Article
fetchArticleHandler aid = do
  marticle <- fetchArticleDB aid
  case marticle of
    Just article -> return article
    Nothing      -> error "Could not fetch article with ID"

createArticleHandler :: MonadDatabase m => Article -> m Int64
createArticleHandler = createArticleDB

fetchArticleByAuthorHandler :: MonadDatabase m => Int64 -> m [KeyVal Article]
fetchArticleByAuthorHandler = fetchArticlesByAuthorDB

fetchRecentArticlesHandler :: MonadDatabase m => m [(KeyVal User, KeyVal Article)]
fetchRecentArticlesHandler = fetchRecentArticlesDB

-- Servant Client: used in Tests to query programmatically the API

fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64
allUsersClient :: ClientM [KeyVal User]

( fetchUserClient  :<|>
  createUserClient :<|>
  allUsersClient ) = client (Proxy :: Proxy UsersAPI)


fetchArticleClient :: Int64 -> ClientM Article
createArticleClient :: Article -> ClientM Int64
fetchArticleByAuthorClient :: Int64 -> ClientM [KeyVal Article]
fetchRecentArticlesClient :: ClientM [(KeyVal User, KeyVal Article)]

( fetchArticleClient          :<|>
  createArticleClient         :<|>
  fetchArticleByAuthorClient  :<|>
  fetchRecentArticlesClient ) = client (Proxy :: Proxy ArticlesAPI)
