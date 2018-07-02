-- TODO: Kill file
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TwilioServer where


import           Control.Monad.IO.Class   (liftIO)
import qualified Data.HashMap.Strict      as HM
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API              ((:<|>), (:>), FormUrlEncoded, Get,
                                           JSON, Post, ReqBody)
import           Servant.Server           (Handler, hoistServer)
import           System.Environment       (getEnv)
import           Twilio
import qualified Twilio.Messages          as TM
import           Web.FormUrlEncoded       (Form (..), FromForm (..))


-- Config
fetchSid :: IO String
fetchSid = getEnv "TWILIO_ACCOUT_SID"

fetchToken :: IO String
fetchToken = getEnv "TWILIO_AUTH_TOKEN"

twilioNum :: Text
twilioNum = "+17027490113"

twilioTestNum :: Text
twilioTestNum = "+15005550006"

bodyMsg :: Text
bodyMsg = "Hello World Twilio!"

-- TODO: replace this with your real number
myNumber :: Text
myNumber = "+123456789"

-- Send a message with twilio
twilioMessage :: Text -> Text -> Text -> Twilio ()
twilioMessage from to bdy = do
  let msg = TM.PostMessage to from bdy
  result <- TM.post msg
  liftIO $ print result
  return ()

-- runTwilio’ fetchSid fetchToken $ twilioMessage twilioNum myNumber bodyMsg
-- > Success!


-- Receiving messages from Twilio
data IncomingMessage = IncomingMessage
  { fromNumber :: Text
  , body       :: Text
  }
  deriving Show

instance FromForm IncomingMessage where
  fromForm :: Form -> Either Text IncomingMessage
  fromForm (Form form) =
    case lookupResults of
      Just ((fromNum : _), (bdy : _)) ->
        Right $ IncomingMessage fromNum bdy

      Just _ ->
        Left "Found the keys but no values"

      Nothing ->
        Left "Didn’t find keys"

    where
      lookupResults :: Maybe ([Text], [Text])
      lookupResults = do
        fromNum <- HM.lookup "From" form
        bdy     <- HM.lookup "Body" form
        return (fromNum, bdy)

-- API
type TwilioServerAPI =
  "api" :> "ping" :> Get '[JSON] String :<|>
  "api" :> "sms" :> ReqBody '[FormUrlEncoded] IncomingMessage :> Post '[JSON] ()

twilioAPI :: Proxy TwilioServerAPI
twilioAPI = Proxy :: Proxy TwilioServerAPI

twilioServer :: Server TwilioServerAPI
twilioServer =
  hoistServer twilioAPI twilioToHandler
  ( pingHandler :<|>
    smsHandler
  )

pingHandler :: Twilio String
pingHandler = return "pong"

smsHandler :: IncomingMessage -> Twilio ()
smsHandler msg = do
  -- TODO: User a Log like monad logger instead
  liftIO $ putStrLn $ "Incoming SMS: " ++ show msg
  let newMsg = TM.PostMessage (fromNumber msg) twilioNum (body msg)
  -- TODO: User a Log like monad logger instead
  liftIO $ putStrLn $ "Sending SMS: " ++ show newMsg
  _ <- TM.post newMsg
  return ()

twilioToHandler :: Twilio a -> Handler a
twilioToHandler twilioAction = liftIO $ runTwilio' fetchSid fetchToken twilioAction

runServer :: IO ()
runServer = do
  port <- read <$> getEnv "PORT"  -- Use port in env variable
  run port $ serve twilioAPI twilioServer
