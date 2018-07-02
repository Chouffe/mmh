{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module API
  (runServer)
  where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (FromJSON, Object, Value, parseJSON,
                                           withObject, (.:), (.:?))
import           Data.Aeson.Types         (Parser)
import           Data.Maybe               (maybe)
import           Data.Monoid              ((<>))
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (Settings, defaultSettings,
                                           runSettings, setLogger, setPort)
import           Servant.API
import           Servant.Server
import           System.Environment       (lookupEnv)

import qualified Github
import qualified Logger

import           Internal                 (Environment (..))

data Config
    = Config
        { cPort        :: Int
        , cEnvironment :: Environment
        , cGithub      :: Github.Config
        , cLogger      :: Logger.Config
        }
    deriving Show

fetchConfig :: IO Config
fetchConfig = do
  port         <- maybe 8091 read <$> lookupEnv "PORT"
  env          <- maybe Development read <$> lookupEnv "ENVIRONMENT"
  githubConfig <- Github.fetchConfig env
  loggerConfig <- Logger.fetchConfig env
  return $ Config
    { cPort = port
    , cEnvironment = env
    , cGithub = githubConfig
    , cLogger = loggerConfig
    }

data GithubRequest
    = GithubOpenPRequest Text Text
    | GithubOtherRequest
    deriving Show

type WebhookAPI =
  "health" :> Get '[JSON] Text :<|>
  "api" :> "hook" :> ReqBody '[JSON] GithubRequest :> Post '[JSON] Text

webhookAPI :: Proxy WebhookAPI
webhookAPI = Proxy :: Proxy WebhookAPI

instance FromJSON GithubRequest where
    parseJSON = withObject "GithubRequest" $ \o -> do
        (action :: Maybe Text)      <- o .: "action"
        (mPRSection :: Maybe Value) <- o .:? "Pull_request"
        case (action, mPRSection) of
          (Just "opened", Just prSection) -> do
              (userSection :: Value, comments :: Text) <-
                  withObject "UserSection" fetchUserAndComments prSection
              userName <- withObject "UserSection" (\o' -> o' .: "login") userSection
              return $ GithubOpenPRequest userName comments
          _ -> return GithubOtherRequest

      where
          fetchUserAndComments :: (FromJSON a, FromJSON b) => Object -> Parser (a, b)
          fetchUserAndComments o' = do
              uSection    <- o' .: "user"
              commentsURL <- o' .: "comments_url"
              return (uSection, commentsURL)

-- Handlers

webhookHandler :: Logger.Handle ->  GithubRequest -> Handler Text
webhookHandler h GithubOtherRequest = do
    liftIO $ Logger.debug h ("Found a non-PR opening request" :: Text)
    return "Found a non-PR opening request"
webhookHandler h req@(GithubOpenPRequest userName commentsURL) = do
    liftIO $ Logger.debug h $ "Received request\n\n: " <> show req  <> "\n\n"
    liftIO $ addComment userName commentsURL
    return $ "User: " <> userName <> " opened a PR with comments at: " <> commentsURL

-- TODO
addComment :: Text -> Text -> IO ()
addComment _ _ = return ()

healthHandler :: Logger.Handle -> Handler Text
healthHandler h = do
  liftIO $ Logger.info h ("Health Ping" :: Text)
  return "OK"

-- Server

webhookServer :: Logger.Handle -> Server WebhookAPI
webhookServer h = ( healthHandler h :<|> webhookHandler h )

-- Running Server

runServer :: IO ()
runServer = do

  -- Fetching Config
  config <- fetchConfig

  Logger.withHandle (cLogger config) $ \h -> do

    Logger.info h (show config)

    -- Running Webserver
    runSettings (mkSettings h config)
      $ serve webhookAPI
      $ webhookServer h

  where
    mkSettings :: Logger.Handle -> Config -> Settings
    mkSettings h@Logger.Handle {..} config =
      setLogger (\request status _ ->
        Logger.info h ("[Request] " <> show request)
        >> Logger.info h ("[Status] " <> show status))
      $ setPort (cPort config)
      $ defaultSettings
