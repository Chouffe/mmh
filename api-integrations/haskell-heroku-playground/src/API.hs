{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module API
  ( runServer
  , fetchConfig
  )
  where

import           Control.Monad            (void)
import           Control.Monad.Freer      (Eff, runM)
import           Control.Monad.IO.Class   (liftIO)
import           Data.ByteString.Char8    (pack)
import           Data.Maybe               (maybe)
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API              ((:<|>), (:>), FormUrlEncoded, Get,
                                           JSON, Post, ReqBody)
import           Servant.Server           (Handler, hoistServer)
import           System.Environment       (lookupEnv)

import           Config

data Config
  = Config
  { cPort :: Int
  , cEnv  :: Environment
  } deriving Show

data Handle
  = Handle
    { hConfig   :: Config
    }

fetchConfig :: Environment -> IO Config
fetchConfig env = do
  port <- maybe 9090 read <$> lookupEnv "PORT"
  return $ Config port env

type ServerAPI =
  "api" :> "ping" :> Get '[JSON] String

fullAPI :: Proxy ServerAPI
fullAPI = Proxy :: Proxy ServerAPI

fullServer :: Handle -> Server ServerAPI
fullServer _ = ( pingHandler )

-- Handlers
pingHandler :: Handler String
pingHandler = return "pong"

runServer :: Environment -> IO ()
runServer env = do

  -- Fetching configuration
  config@Config{..} <- fetchConfig env

  -- Debugging
  putStrLn $ "config: " ++ show config

  run cPort $ serve fullAPI $ fullServer (Handle config)
