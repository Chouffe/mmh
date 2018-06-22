{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
  (runServer)
  where


import           Control.Monad            (void)
import           Control.Monad.Freer      (Eff, runM)
import           Control.Monad.IO.Class   (liftIO)
import           Data.ByteString.Char8    (pack)
-- import           Data.Either              (either)
import qualified Data.HashMap.Strict      as HM
import           Data.Proxy               (Proxy (..))
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API              ((:<|>), (:>), FormUrlEncoded, Get,
                                           JSON, Post, ReqBody)
import           Servant.Server           (Handler, hoistServer)
import           System.Environment       (getEnv)

import           Eff.Email                (Email (..), runEmail,
                                           sendSubscribeEmail)
import qualified Eff.Email
import           Eff.SMS                  (SMS (..), SMSCommand (..),
                                           messageToSMSCommand, runSMS,
                                           sendText)
import qualified Eff.SMS
import           Types                    (IncomingMessage (..))

data Config = Config
  { cPort :: Int
  }
  deriving Show

fetchConfig :: IO Config
fetchConfig = do
  port <- read <$> getEnv "PORT"
  return $ Config port

type AppEff = Eff '[SMS, Email, IO]

runAppEff :: Eff.Email.Config -> AppEff a -> IO a
runAppEff emailConfig eff = runM
  $ runEmail emailConfig
  $ runSMS Eff.SMS.fetchSid Eff.SMS.fetchToken eff

-- TODO: improve Error handling
appEffToHandler :: Eff.Email.Config -> AppEff a -> Handler a
appEffToHandler emailConfig eff = liftIO $ runAppEff emailConfig eff

type ServerAPI =
  "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] ()

fullAPI :: Proxy ServerAPI
fullAPI = Proxy :: Proxy ServerAPI

fullServer :: Eff.Email.Config -> Server ServerAPI
fullServer emailConfig =
  hoistServer fullAPI (appEffToHandler emailConfig)
  ( pingHandler :<|>
    smsHandler
  )

-- Handlers
pingHandler :: AppEff String
pingHandler = return "pong"

smsHandler :: IncomingMessage -> AppEff ()
smsHandler = subscribeViaSMS

-- Somme commands in the Eff monad
subscribeViaSMS :: IncomingMessage -> AppEff ()
subscribeViaSMS msg = do
  case messageToSMSCommand (body msg) of
    Nothing -> sendText (fromNumber msg) "Sorry we did not understand the request..."
    Just (SubscribeCommand email) -> void $ sendSubscribeEmail (pack (show email))

-- echoSMS :: IncomingMessage -> AppEff ()
-- echoSMS msg = void $ sendText (fromNumber msg) (body msg)

-- incomingMessage :: IncomingMessage
-- incomingMessage = IncomingMessage "+12345" "Echo Echo Echo..."  -- User a test number here

-- sendSimpleEmail :: AppEff ()
-- sendSimpleEmail = either (const ()) (const ()) <$> sendSubscribeEmail "verified@mail.com"

runServer :: IO ()
runServer = do

  -- Fetching configuration
  serverConfig <- fetchConfig
  emailConfig <- Eff.Email.fetchEmailConfig

  -- Debugging
  putStrLn $ "Email Config: " ++ show emailConfig
  putStrLn $ "Running Server with Config: " ++ show serverConfig

  -- Running Servant Server
  run (cPort serverConfig) $ serve fullAPI (fullServer emailConfig)
