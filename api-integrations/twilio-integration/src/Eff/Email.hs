{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE RecordWildCards    #-}

module Eff.Email
  ( Config (..)
  , fetchConfig

  , Email
  , EmailContent
  , EmailAddress

  , sendSubscribeEmail
  , sendEmailToList
  , runEmail
  )
  where

-- import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Freer.Extended    (Eff, Member, send, runNat)
import           Data.Monoid                     ((<>))
import           Data.ByteString.Char8           (pack)
import           Data.ByteString                 (ByteString)
import           Data.Text                       (Text)
import           Data.Text.Encoding (encodeUtf8)
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

type EmailContent = (Text, ByteString, Maybe ByteString)
type EmailAddress = Text

fetchConfig :: IO Config
fetchConfig = do
  dom         <- getEnv "MAILGUN_DOMAIN"
  key         <- getEnv "MAILGUN_API_KEY"
  replyAddress <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"
  currentDir <- getCurrentDirectory
  return $ Config dom key replyAddress currentDir

data Email a where
  SendSubscribeEmail :: ByteString -> Email (Either String ())
  SendEmailToList    :: EmailContent -> [EmailAddress] -> Email (Either String ())

deriving instance Show (Email a)

sendSubscribeEmail :: (Member Email r) => ByteString -> Eff r (Either String ())
sendSubscribeEmail = send . SendSubscribeEmail

sendEmailToList :: (Member Email r) => EmailContent -> [EmailAddress] -> Eff r (Either String ())
sendEmailToList emailMessage emailAddresses = send $ SendEmailToList emailMessage emailAddresses

-- TODO: Improve with a Logger
runEmail :: (Member IO r) => Config -> Eff (Email ': r) a -> Eff r a
runEmail config@Config{..} = runNat emailToIO

  where

    context :: HailgunContext
    context = HailgunContext cDomain cApiKey Nothing

    -- TODO: Add proper logging
    sendWithCheck :: HailgunContext -> Either String HailgunMessage -> IO (Either String ())
    sendWithCheck ctx eitherMsg = case eitherMsg of
      Left err -> return $ Left err
      Right msg -> do
        result <- sendEmail ctx msg
        case result of
          Left err -> return $ Left (show err)
          Right _ -> return $ Right ()

    emailToIO :: Email a -> IO a
    emailToIO (SendSubscribeEmail to) =
      sendWithCheck context (mkSubscribeMessage config to)

    emailToIO (SendEmailToList content subscribers) =
      sendWithCheck context (mkListMessage cReplyAddress content subscribers)


mkSubscribeMessage
  :: Config
  -> ByteString
  -> Either HailgunErrorMessage HailgunMessage
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

mkListMessage
  :: ByteString
  -> (Text, ByteString, Maybe ByteString)
  -> [Text]
  -> Either HailgunErrorMessage HailgunMessage
mkListMessage replyAddress (subject, txtOnly, maybeHTML) subscribers =
  hailgunMessage
    subject
    finalContent
    replyAddress
    (emptyMessageRecipients { recipientsBCC = map encodeUtf8 subscribers })
    []
  where
    finalContent :: MessageContent
    finalContent = case maybeHTML of
      Nothing -> TextOnly txtOnly
      Just html -> TextAndHTML txtOnly html
