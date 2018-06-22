{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Eff.SMS
  ( runSMS
  , messageToSMSCommand
  , SMS (..)
  , SMSCommand (..)
  , sendText
  , twilioNum
  , twilioTestNum
  , myNumber
  , fetchSid
  , fetchToken
  )
  where

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Freer.Extended  (Eff, Member, send, runNat)
import           Data.Text                     (Text, splitOn)
import           Twilio
import qualified Twilio.Messages               as TM
import           System.Environment            (getEnv)


-- Configs
fetchSid :: IO String
fetchSid = getEnv "TWILIO_ACCOUT_SID"

fetchToken :: IO String
fetchToken = getEnv "TWILIO_AUTH_TOKEN"

twilioNum :: Text
twilioNum = "+17027490113"

twilioTestNum :: Text
twilioTestNum = "+15005550006"

-- TODO: replace this with your real number
myNumber :: Text
myNumber = "+123456789"

-- Can hold more commands as a Sum Type
data SMSCommand = SubscribeCommand Text

messageToSMSCommand :: Text -> Maybe SMSCommand
messageToSMSCommand messageBody =
  case splitOn " " messageBody of
    ["subscribe", email] -> Just $ SubscribeCommand email
    _                    -> Nothing


data SMS a where
  SendText :: Text -> Text -> SMS ()

deriving instance Show (SMS a)

sendText :: (Member SMS r) => Text -> Text -> Eff r ()
sendText fromNumber body = send $ SendText fromNumber body

runSMS :: (Member IO r) => IO String -> IO String -> Eff (SMS ': r) a -> Eff r a
runSMS sidFetcher authFetcher = runNat smsToIO

  where
    smsToIO :: SMS a -> IO a
    smsToIO (SendText fromNumber body) =
      runTwilio' sidFetcher authFetcher $ do
        let twilioMsg = TM.PostMessage fromNumber twilioNum body
        liftIO $ putStrLn $ "Sending SMS: " ++ show twilioMsg
        _ <- TM.post twilioMsg
        return ()
