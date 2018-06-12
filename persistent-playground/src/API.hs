{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module API where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int                   (Int64)
import           Data.Proxy                 (Proxy (..))
import           Database.Persist           (Entity (..))
import           Network.Wai.Handler.Warp   (run)
import           Servant.API
import           Servant.Client             (ClientM, client)
import           Servant.Server

import           Database
import           Schema

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
  :<|> "users" :> "all" :> Get '[JSON] [User]

-- Articles API
type ArticlesAPI
  = "articles" :> Capture "articleid" Int64 :> Get '[JSON] Article
  :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Int64
  :<|> "articles" :> "author" :> Capture "authorid" Int64 :> Get '[JSON] [Entity Article]
  :<|> "articles" :> "recent" :> Get '[JSON] [(Entity User, Entity Article)]

type FullAPI
  = UsersAPI
  :<|> ArticlesAPI

usersServer :: SQLiteInfo -> RedisInfo -> Server UsersAPI
usersServer sqlInfo redInfo =
       fetchUserHandler sqlInfo redInfo
  :<|> createUserHandler sqlInfo
  :<|> allUsersHandler sqlInfo

articlesServer :: SQLiteInfo -> Server ArticlesAPI
articlesServer sqlInfo =
       fetchArticleHandler sqlInfo
  :<|> createArticleHandler sqlInfo
  :<|> fetchArticleByAuthorHandler sqlInfo
  :<|> fetchRecentArticlesHandler sqlInfo

fullServer :: SQLiteInfo -> RedisInfo -> Server FullAPI
fullServer sqlInfo redInfo =
       usersServer sqlInfo redInfo
  :<|> articlesServer sqlInfo

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

  -- Debug
  putStrLn "Fetching all users in DB"
  users <- runSqliteAction (sqliteInfo config) selectYoungUsers
  print users

  putStrLn $ "Running server on port: " ++ (show (port config))
  run (port config)
    $ serve fullAPI
    $ fullServer (sqliteInfo config) (redisInfo config)

-- Handlers
fetchUserHandler :: SQLiteInfo -> RedisInfo -> Int64 -> Handler User
fetchUserHandler connStr redInfo userId = do
  liftIO $ putStrLn "Fetching User"
  mRedisUser <- liftIO $ fetchUserRedis redInfo userId
  case mRedisUser of
    Just user -> liftIO (putStrLn "Cache Hit!") >> return user
    Nothing -> do
      mDBuser <- liftIO $ fetchUserSQLite connStr userId
      case mDBuser of
        Nothing   -> Handler $ (throwE $ err401 { errBody = "Could not find user with ID"})
        Just user -> do
          liftIO $ cacheUser redInfo userId user
          return user

createUserHandler :: SQLiteInfo -> User -> Handler Int64
createUserHandler connStr user = do
  liftIO $ putStrLn "Creating user"
  liftIO $ createUserSQLite connStr user

allUsersHandler :: SQLiteInfo -> Handler [User]
allUsersHandler connStr = liftIO $ getAllUsersSQLite connStr

fetchArticleHandler :: SQLiteInfo -> Int64 -> Handler Article
fetchArticleHandler connStr aid = do
  maybeArticle <- liftIO $ fetchArticleSQLite connStr aid
  case maybeArticle of
    Just article -> return article
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find article with that ID" })

createArticleHandler :: SQLiteInfo -> Article -> Handler Int64
createArticleHandler connStr article = liftIO $ createArticleSQLite connStr article

fetchArticleByAuthorHandler :: SQLiteInfo -> Int64 -> Handler [Entity Article]
fetchArticleByAuthorHandler connStr uid = liftIO $ fetchArticleByAuthorSQLite connStr uid

fetchRecentArticlesHandler :: SQLiteInfo -> Handler [(Entity User, Entity Article)]
fetchRecentArticlesHandler connStr = liftIO $ fetchRecentArticlesSQLite connStr 10

-- Servant Client: used in Tests to query programmatically the API

fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64
allUsersClient :: ClientM [User]

fetchUserClient :<|>
  createUserClient :<|>
    allUsersClient = client (Proxy :: Proxy UsersAPI)
