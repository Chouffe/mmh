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
import           Network.Wai.Handler.Warp   (run)
import           Servant.API
import           Servant.Client             (ClientEnv (..), ClientM, client)
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
type UsersAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "all" :> Get '[JSON] [User]

usersServer :: SQLiteInfo -> RedisInfo -> Server UsersAPI
usersServer sqlInfo redInfo =
       fetchUserHandler sqlInfo redInfo
  :<|> createUserHandler sqlInfo
  :<|> allUsersHandler sqlInfo


usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

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
    $ serve usersAPI
    $ usersServer (sqliteInfo config) (redisInfo config)

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

-- Servant Client

fetchUserClient :: Int64 -> ClientM User
createUserClient :: User -> ClientM Int64
allUsersClient :: ClientM [User]

fetchUserClient :<|>
  createUserClient :<|>
    allUsersClient = client (Proxy :: Proxy UsersAPI)
