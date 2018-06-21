{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import           System.Environment     (getEnv)
import           Twilio
import qualified Twilio.Messages        as TM


fetchSid :: IO String
fetchSid = getEnv "TWILIO_ACCOUT_SID"

fetchToken :: IO String
fetchToken = getEnv "TWILIO_AUTH_TOKEN"

twilioNum :: Text
twilioNum = "+17027490113"

twilioTestNum :: Text
twilioTestNum = "+15005550006"

myNumber :: Text
myNumber = "+491731630137"

bodyMsg :: Text
bodyMsg = "Hello World Twilio!"

-- Run the Twilio Monad
-- runTwilio’ :: IO String -> IO String -> Twilio a -> IO a

twilioMessage :: Text -> Text -> Text -> Twilio ()
twilioMessage from to bdy = do
  let msg = TM.PostMessage to from bdy
  result <- TM.post msg
  liftIO $ print result
  return ()

-- runTwilio’ fetchSid fetchToken $ twilioMessage twilioNum myNumber bodyMsg
-- > Success!

sendMessage :: IO ()
sendMessage = runTwilio' fetchSid fetchToken $ do
  let msg = TM.PostMessage myNumber twilioTestNum "Hello Twilio"
  result <- TM.post msg
  liftIO $ print result
  return ()

-- Receiving messages from Twilio
data IncomingMessage = IncomingMessage
  { fromNumber :: Text
  , body       :: Text
  }
