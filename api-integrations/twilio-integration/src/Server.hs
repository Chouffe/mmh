{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Server
  ( runServer
  , fetchConfig
  )
  where


import           Control.Monad            (void)
import           Control.Monad.Freer      (Eff, runM)
import           Control.Monad.IO.Class   (liftIO)
import           Data.ByteString.Char8    (pack)
import qualified Data.HashMap.Strict      as HM
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API              ((:<|>), (:>), FormUrlEncoded, Get,
                                           JSON, Post, ReqBody)
import           Servant.Server           (Handler, hoistServer)
import           System.Environment       (getEnv)

import           Eff.Database             (Database, migrateDB, registerUser,
                                           runDatabase)
import qualified Eff.Database

import           Eff.Email                (Email, runEmail, sendSubscribeEmail)
import qualified Eff.Email
import           Eff.SMS                  (SMS, SMSCommand (..),
                                           messageToSMSCommand, runSMS,
                                           sendText)
import qualified Eff.SMS
import           Types

data Config
  = Config
  { cPort     :: Int
  , cDatabase :: Eff.Database.Config
  , cEmail    :: Eff.Email.Config
  , cSMS      :: Eff.SMS.Config
  , cEnv      :: Environment
  } deriving Show

data Handle
  = Handle
    { hConfig   :: Config
    , hDatabase :: Eff.Database.Handle
    }

fetchConfig :: Environment -> IO Config
fetchConfig env = do
  -- Add environment (test, dev, prod) switch
  port           <- read <$> getEnv "PORT"
  emailConfig    <- Eff.Email.fetchConfig
  databaseConfig <- Eff.Database.fetchConfig
  smsConfig      <- Eff.SMS.fetchConfig
  return $ Config port databaseConfig emailConfig smsConfig env

type AppEff = Eff '[Database, SMS, Email, IO]

runAppEff :: Handle -> AppEff a -> IO a
runAppEff Handle{..} eff = runM
  $ runEmail cEmail
  $ runSMS (Eff.SMS.fetchSid cSMS) (Eff.SMS.fetchToken cSMS)
  $ runDatabase hDatabase
  $ eff

  where
    config@Config{..} = hConfig

-- TODO: improve Error handling
appEffToHandler :: Handle -> AppEff a -> Handler a
appEffToHandler handle eff = liftIO $ runAppEff handle eff

type ServerAPI =
  "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] () :<|>
  "api" :> "subscribe" :> Capture "email" Text :> Get '[JSON] String

fullAPI :: Proxy ServerAPI
fullAPI = Proxy :: Proxy ServerAPI

fullServer :: Handle -> Server ServerAPI
fullServer handle =
  hoistServer fullAPI (appEffToHandler handle)
  ( pingHandler      :<|>
    smsHandler       :<|>
    subscribeHandler
  )

-- Handlers
pingHandler :: AppEff String
pingHandler = return "pong"

smsHandler :: IncomingMessage -> AppEff ()
smsHandler = subscribeViaSMS

subscribeHandler :: Text -> AppEff String
subscribeHandler email = do
  -- TODO: add a check for email input
  registerUser email
  return $ "Subscribed with email: " ++ show email

-- Somme commands in the Eff monad
subscribeViaSMS :: IncomingMessage -> AppEff ()
subscribeViaSMS msg = do
  case messageToSMSCommand (body msg) of
    Nothing -> sendText (fromNumber msg) "Sorry we did not understand the request..."
    Just (SubscribeCommand email) -> void $ sendSubscribeEmail (pack (show email))

-- sendEmailToList :: EmailContent -> AppEff ()
-- sendEmailToList emailContent = do
--   subscribers <- Eff.Database.retrieveSubscribers
--   errorOrSent <- Eff.Email.sendEmailToList undefined subscribers
--   case errorOrSent of
--     Left err -> liftIO $ putStrLn (show err)
--     Right _  -> return ()


-- echoSMS :: IncomingMessage -> AppEff ()
-- echoSMS msg = void $ sendText (fromNumber msg) (body msg)

-- incomingMessage :: IncomingMessage
-- incomingMessage = IncomingMessage "+12345" "Echo Echo Echo..."  -- User a test number here

-- sendSimpleEmail :: AppEff ()
-- sendSimpleEmail = either (const ()) (const ()) <$> sendSubscribeEmail "verified@mail.com"

runServer :: Environment -> IO ()
runServer env = do

  -- Fetching configuration
  config@Config{..} <- fetchConfig env

  -- Debugging
  putStrLn $ "config: " ++ show config

  -- DB Migration
  Eff.Database.withHandle cEnv cDatabase $ \hDatabase -> do

    -- Creating the global handle
    let handle = Handle config hDatabase

    -- Migrating schemas
    migrateDB hDatabase

    -- Running Servant Server
    run cPort $ serve fullAPI $ fullServer handle
