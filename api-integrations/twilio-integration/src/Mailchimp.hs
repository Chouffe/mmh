{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Mailchimp where

import           Data.Aeson              (FromJSON, ToJSON, object, parseJSON,
                                          toJSON, withObject, (.:), (.=))
import           Data.ByteString.Char8   (pack)
import           Data.List               (find)
import           Data.Proxy              (Proxy (..))
import           Data.Text               (Text)
import           Network.HTTP.Client.TLS (newTlsManager)
import           Servant.API
import           Servant.API.BasicAuth   (BasicAuthData)
import           Servant.Client
import           System.Environment      (getEnv)



type MCAuth = BasicAuth "mailchimp" ()

type MailchimpAPI =
  MCAuth :> "lists" :> Get '[JSON] MailchimpListResponse :<|>
  MCAuth :> "lists" :> Capture "list-id" Text :> "members" :> QueryParam "count" Int :> Get '[JSON] MailchimpMembersResponse :<|>
  MCAuth :> "lists" :> Capture "list-id" Text :> "members" :> ReqBody '[JSON] MailchimpSubscriber :> Post '[JSON]  ()

mailchimpAPI :: Proxy MailchimpAPI
mailchimpAPI = Proxy :: Proxy MailchimpAPI

fetchListsClient       :: BasicAuthData -> ClientM MailchimpListResponse
fetchSubscribersClient :: BasicAuthData -> Text -> Maybe Int -> ClientM MailchimpMembersResponse
subscribeNewUserClient :: BasicAuthData -> Text -> MailchimpSubscriber -> ClientM ()

( fetchListsClient       :<|>
  fetchSubscribersClient :<|>
  subscribeNewUserClient ) = client mailchimpAPI

data Config
  = Config
    { cBaseUrl :: String
    , cApiKey  :: String
    }
  deriving Show

fetchConfig :: IO Config
fetchConfig = do
  baseUrl_ <- getEnv "MAILCHIMP_BASE_URL"
  apiKey   <- getEnv "MAILCHIMP_API_KEY"
  return $ Config baseUrl_ apiKey

-- TODO: should use a handle instead of creating a tls client every single time...
runMailchimp :: Config -> (BasicAuthData -> ClientM a) -> IO (Either ServantError a)
runMailchimp Config{..} action = do
  tlsManager <- newTlsManager
  fullUrl    <- parseBaseUrl cBaseUrl
  let clientEnv = mkClientEnv tlsManager fullUrl
  runClientM (action userData) clientEnv

  where
    userData :: BasicAuthData
    userData = BasicAuthData "username" (pack cApiKey)

-- We use newtype to add a From/To JSON instance
newtype MailchimpSimpleSingleList
  = MailchimpSimpleSingleList (Text, Text)
  deriving Show

newtype MailchimpListResponse
  = MailchimpListResponse [MailchimpSimpleSingleList]
  deriving Show

newtype MailchimpSubscriber
  = MailchimpSubscriber { unMailchimpSubscriber :: Text }
  deriving Show

newtype MailchimpMembersResponse
  = MailchimpMembersResponse [MailchimpSubscriber]
  deriving Show

instance FromJSON MailchimpSimpleSingleList where
  parseJSON = withObject "MailchimpSimpleSingleList" $ \o -> do
    name <- o .: "name"
    id_  <- o .: "id"
    return $ MailchimpSimpleSingleList (name, id_)

instance FromJSON MailchimpListResponse where
  parseJSON = withObject "MailchimpListResponse" $ \o -> do
    lists <- o .: "lists"
    xs    <- mapM parseJSON lists
    return $ MailchimpListResponse xs

instance FromJSON MailchimpSubscriber where
  parseJSON = withObject "MailchimpSubscriber" $ \o -> do
    email <- o .: "email_address"
    return $ MailchimpSubscriber email

instance FromJSON MailchimpMembersResponse where
  parseJSON = withObject "MailchimpMembersResponse" $ \o -> do
    members     <- o .: "members"
    subscribers <- mapM parseJSON members
    return $ MailchimpMembersResponse subscribers

instance ToJSON MailchimpSubscriber where
  toJSON (MailchimpSubscriber email) = object
    [ "email_address".= email
    , "status"       .= ("subscribed" :: Text)
    ]

-- Functions to call the Mailchimp API

getListId :: IO ()
getListId = do
  config         <- fetchConfig
  mailchimpLists <- runMailchimp config fetchListsClient
  print mailchimpLists

fetchMCListId :: Text -> IO (Either String Text)
fetchMCListId listName = do
  config        <- fetchConfig
  listsResponse <- runMailchimp config fetchListsClient
  case listsResponse of
    Left err -> return $ Left (show err)
    Right (MailchimpListResponse lists) ->
      case find nameMatches lists of
        Nothing -> return $ Left "Could not find list with that name!"
        Just (MailchimpSimpleSingleList (_, id_)) -> return $ Right id_
  where
    nameMatches :: MailchimpSimpleSingleList -> Bool
    nameMatches (MailchimpSimpleSingleList (name, _)) = name == listName

fetchMCListMembers :: Text -> IO (Either String [Text])
fetchMCListMembers listId = do
  config <- fetchConfig
  membersResponse <- runMailchimp config (\auth -> fetchSubscribersClient auth listId (Just 2000))
  case membersResponse of
    Left err -> return $ Left (show err)
    Right (MailchimpMembersResponse subs) -> return $ Right $ map unMailchimpSubscriber subs

subscribeMCMember :: Text -> Text -> IO (Either String ())
subscribeMCMember listId email = do
  config <- fetchConfig
  subscribeResponse <- runMailchimp config (\auth -> subscribeNewUserClient auth listId (MailchimpSubscriber email))
  case subscribeResponse of
    Left err -> return $ Left (show err)
    Right _  -> return $ Right ()
