{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Github
  ( Config(..)
  , fetchConfig

  , fetchUserClient
  , fetchUserReposClient

  , action
  )
  where

import           Data.Monoid             ((<>))
import           Data.Proxy
import           Data.Text               (Text)
import           Data.Text               as T
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           System.Environment      (getEnv)

import           Internal                (Environment (..))

data Config
    = Config
      {
        token :: Text
      }
    deriving Show

mkToken :: String -> Text
mkToken token =  T.pack $ "token " <> token

fetchConfig :: Environment -> IO Config
fetchConfig _ = do
    token <- getEnv "GITHUB_AUTH_TOKEN"
    return $ Config $ mkToken token

type GithubAPI
  = Header "User-Agent" Text :> Header "Authorization" Text :> "user" :> Get '[JSON] ()
  :<|> Header "User-Agent" Text :> Header "Authorization" Text :> "user" :> "repos" :> Get '[JSON] ()

-- Client

fetchUserClient        :: Maybe Text -> Maybe Text -> ClientM ()
fetchUserReposClient   :: Maybe Text -> Maybe Text -> ClientM ()
-- addCommentClient       :: Maybe Text -> Maybe Text -> ClientM ()

( fetchUserClient :<|>
  fetchUserReposClient ) = client (Proxy :: Proxy GithubAPI)


action :: IO ()
action = do
  -- TODO: fetch config from here
  config     <- fetchConfig Development
  mgr        <- newManager tlsManagerSettings
  baseUrl    <- parseBaseUrl $ "https://api.github.com/"
  print config

  let clientEnv = mkClientEnv mgr baseUrl

  result <- runClientM (fetchUserClient (Just "Octocat-App") (Just (token config))) clientEnv
  print result
  return ()
