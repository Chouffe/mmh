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
-- import           Data.Either              (either)
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
import           Types                    (IncomingMessage (..))

data Config = Config
  { cPort     :: Int
  , cDatabase :: Eff.Database.Config
  , cEmail    :: Eff.Email.Config
  , cSMS      :: Eff.SMS.Config
  } deriving Show

fetchConfig :: IO Config
fetchConfig = do
  port           <- read <$> getEnv "PORT"
  emailConfig    <- Eff.Email.fetchConfig
  databaseConfig <- Eff.Database.fetchConfig
  smsConfig      <- Eff.SMS.fetchConfig
  return $ Config port databaseConfig emailConfig smsConfig

type AppEff = Eff '[Database, SMS, Email, IO]

runAppEff :: Config -> AppEff a -> IO a
runAppEff Config {..} eff = runM
  $ runEmail cEmail
  $ runSMS (Eff.SMS.fetchSid cSMS) (Eff.SMS.fetchToken cSMS)
  $ runDatabase cDatabase
  $ eff

-- TODO: improve Error handling
appEffToHandler :: Config -> AppEff a -> Handler a
appEffToHandler config eff = liftIO $ runAppEff config eff

type ServerAPI =
  "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] () :<|>
  "api" :> "subscribe" :> Capture "email" Text :> Get '[JSON] String

fullAPI :: Proxy ServerAPI
fullAPI = Proxy :: Proxy ServerAPI

fullServer :: Config -> Server ServerAPI
fullServer config =
  hoistServer fullAPI (appEffToHandler config)
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

runServer :: IO ()
runServer = do

  -- Fetching configuration
  config <- fetchConfig

  -- DB Migration
  migrateDB (cDatabase config)

  -- Debugging
  putStrLn $ "config: " ++ show config

  -- Running Servant Server
  run (cPort config)
    $ serve fullAPI
    $ fullServer config
