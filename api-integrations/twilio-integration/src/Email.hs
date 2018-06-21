{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Email where

import           Data.ByteString.Char8 (pack)
import           Mail.Hailgun
import           System.Environment    (getEnv)

-- sendEmail :: HailgunContext
--           -> HailgunMessage
--           -> IO (Either HailgunErrorResponse HailgunSendResponse)
--

createAndSendMail :: IO ()
createAndSendMail = do
  domain       <- getEnv "MAILGUN_DOMAIN"
  apiKey       <- getEnv "MAILGUN_API_KEY"
  replyAddress <- pack <$> getEnv "MAILGUN_REPLY_ADDRESS"

  --  Debug
  putStrLn domain
  putStrLn apiKey
  putStrLn (show replyAddress)

  let context = HailgunContext domain apiKey Nothing

  case (mkMessage replyAddress) of
    Left e -> putStrLn $ "Making failed: " ++ show e
    Right msg -> do
      result <- sendEmail context msg
      case result of
        Left err   -> putStrLn $ "Sending failed" ++ show err
        Right resp -> putStrLn $ "Sending successful: " ++ show resp

  where
    mkMessage :: UnverifiedEmailAddress -> Either HailgunErrorMessage HailgunMessage
    mkMessage replyAddr = hailgunMessage
      "Hello Mailgun"                                      -- Subject
      (TextOnly "This is a text message")                  -- Body
      replyAddr                                            -- Reply to
      (emptyMessageRecipients
        { recipientsTo = ["verified@mail.com"] })          -- From: use your verified email
      []                                                   -- Attachments

-- hailgunMessage
--  :: MessageSubject
--   -> MessageContent
--    -> UnverifiedEmailAddress -- Reply Address, just a ByteString
--     -> MessageRecipients
--      -> [Attachment]
--       -> Either HailgunErrorMessage HailgunMessage
