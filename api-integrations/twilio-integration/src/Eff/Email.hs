{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs      #-}

module Eff.Email
  ( runEmail
  , fetchEmailConfig
  , Email (..)
  , Config
  , sendSubscribeEmail
  )
  where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Freer    (Eff, Member, send)
import           Data.ByteString.Char8  (pack)
import           Data.Text              (Text)
import           Mail.Hailgun
import           System.Environment     (getEnv)

import           Eff.Utils              (runNat)

data Config
  = Config
    { domain       :: String
    , apiKey       :: String
    , replyAddress :: UnverifiedEmailAddress
    }
    deriving (Eq, Show)


fetchEmailConfig :: IO Config
fetchEmailConfig = do
  dom         <- getEnv "MAILGUN_DOMAIN"
  key         <- getEnv "MAILGUN_API_KEY"
  rplyAddress <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"
  return $ Config dom key rplyAddress

data Email a where
  SendSubscribeEmail :: Text -> Email (Either String ())

deriving instance Show (Email a)

sendSubscribeEmail :: (Member Email r) => Text -> Eff r (Either String ())
sendSubscribeEmail = send . SendSubscribeEmail

runEmail :: (Member IO r) => Config -> Eff (Email ': r) a -> Eff r a
runEmail config = runNat emailToIO

  where
    emailToIO :: Email a -> IO a
    emailToIO (SendSubscribeEmail to) =
      let context = HailgunContext (domain config) (apiKey config) Nothing
      in
      case mkSubscribeMessage config (show to) of
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


mkSubscribeMessage :: Config
                    -> String
                    -> Either HailgunErrorMessage HailgunMessage
mkSubscribeMessage config to = hailgunMessage
  "Welcome to the Newsletter!"
  (TextOnly "Welcome, Lorem Ipsum this is the body!")
  (replyAddress config)
  (emptyMessageRecipients { recipientsTo = [pack to] })
  []
