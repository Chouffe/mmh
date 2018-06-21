{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}

module Server
  (runServer)
  where


import           Control.Monad            (void)
import           Control.Monad.Freer      (Eff, Member, LastMember, runM)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.HashMap.Strict      as HM
import           Data.Proxy               (Proxy (..))
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API              ((:<|>), (:>), FormUrlEncoded, Get,
                                           JSON, Post, ReqBody)
import           Servant.Server           (Handler, hoistServer)
import           System.Environment       (getEnv)

import qualified Eff.Email
import           Eff.Email                (sendSubscribeEmail, Email (..), runEmail)
import qualified Eff.SMS
import           Eff.SMS                  (sendText, SMS (..), SMSCommand (..), messageToSMSCommand, runSMS)
import           Types                    (IncomingMessage (..))

data Config = Config { port :: Int }
  deriving Show

fetchConfig :: IO Config
fetchConfig = do
  port <- read <$> getEnv "PORT"
  return $ Config port

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
subscribeViaSMS :: (Member SMS r, Member Email r, LastMember IO r) => IncomingMessage -> Eff r ()
subscribeViaSMS msg = do
  case messageToSMSCommand (body msg) of
    Nothing -> sendText (fromNumber msg) "Sorry we did not understand the request..."
    Just (SubscribeCommand email) -> do
      void $ sendSubscribeEmail email

echoSMS :: (Member SMS r, LastMember IO r) => IncomingMessage -> Eff r ()
echoSMS msg = void $ sendText (fromNumber msg) (body msg)

type AppEff = Eff '[SMS, Email, IO]

runAppEff :: Eff.Email.Config -> AppEff a -> IO a
runAppEff emailConfig eff = runM
  $ runEmail emailConfig
  $ runSMS Eff.SMS.fetchSid Eff.SMS.fetchToken eff

-- TODO: improve Error handling
appEffToHandler :: Eff.Email.Config -> AppEff a -> Handler a
appEffToHandler emailConfig eff = liftIO $ runAppEff emailConfig eff

runServer :: IO ()
runServer = do

  -- Fetching configuration
  serverConfig <- fetchConfig
  emailConfig <- Eff.Email.fetchEmailConfig

  -- Debugging
  putStrLn $ "Email Config: " ++ show emailConfig
  putStrLn $ "Running Server with Config: " ++ show serverConfig

  -- Running Servant Server
  run (port serverConfig) $ serve fullAPI (fullServer emailConfig)
