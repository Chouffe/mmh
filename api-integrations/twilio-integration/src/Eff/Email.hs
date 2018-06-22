{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE InstanceSigs       #-}

module Eff.Email
  ( runEmail
  , fetchEmailConfig
  , Email (..)
  , Config
  , sendSubscribeEmail
  )
  where

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Freer.Extended    (Eff, Member, send, runNat)
import           Data.Monoid                     ((<>))
import           Data.ByteString.Char8           (pack)
import           Data.ByteString                 (ByteString)
import           Mail.Hailgun
import           System.Environment              (getEnv)
import           System.Directory                (getCurrentDirectory)


data Config
  = Config
    { cDomain       :: String
    , cApiKey       :: String
    , cReplyAddress :: UnverifiedEmailAddress
    , cCurrentDir   :: FilePath
    }
    deriving (Eq, Show)


fetchEmailConfig :: IO Config
fetchEmailConfig = do
  dom         <- getEnv "MAILGUN_DOMAIN"
  key         <- getEnv "MAILGUN_API_KEY"
  replyAddress <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"
  currentDir <- getCurrentDirectory
  return $ Config dom key replyAddress currentDir

data Email a where
  SendSubscribeEmail :: ByteString -> Email (Either String ())

deriving instance Show (Email a)

sendSubscribeEmail :: (Member Email r) => ByteString -> Eff r (Either String ())
sendSubscribeEmail = send . SendSubscribeEmail

runEmail :: (Member IO r) => Config -> Eff (Email ': r) a -> Eff r a
runEmail config = runNat emailToIO

  where
    emailToIO :: Email a -> IO a
    emailToIO (SendSubscribeEmail to) =
      let context = HailgunContext (cDomain config) (cApiKey config) Nothing
      in do
        liftIO $ putStrLn (show context)
        case mkSubscribeMessage config to of
          Left e -> do
            liftIO $ putStrLn $ "Making failed: " ++ show e
            return $ Left (show e)
          Right msg' -> do
            result <- liftIO $ sendEmail context msg'
            case result of
              Left err -> do
                liftIO $ putStrLn $ "Sending failed: " ++ show err
                return $ Left (show err)
              Right resp -> do
                liftIO $ putStrLn $ "Sending successful: " ++ show resp
                return $ Right ()


mkSubscribeMessage :: Config -> ByteString -> Either HailgunErrorMessage HailgunMessage
mkSubscribeMessage config to =
  hailgunMessage
    "Welcome to the Newsletter!"
    content
    (cReplyAddress config)
    (emptyMessageRecipients { recipientsTo = [to] })
    [ Attachment
      (rewardFilepath (cCurrentDir config))
      (AttachmentBS "Your reward!")
    ]
  where
    -- content :: MessageContent
    -- content = TextOnly "Here's your reward!"
    content :: MessageContent
    content = TextAndHTML
       textOnly
          (  "Here's your reward! To confirm your subscription, click "
          <> link
          <> "!"
          )

    textOnly = "Here's your reward! To confirm your subscription, go to "
      <> "https://haskell-apis.herokuapp.com/api/subscribe/"
      <> to
      <> " and we'll sign you up!"

    link = "<a href=\"https://haskell-apis.herokuapp.com/api/subscribe/"
      <> to
      <> "\">this link</a>"

    rewardFilepath :: FilePath -> FilePath
    rewardFilepath fp = fp ++ "/attachments/reward.txt"
