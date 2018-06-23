{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Eff.SubscriberList
  ( SubscriberList
  , fetchListId
  , fetchListMembers
  , subscribeMember
  , runSubscriberList
  )
  where

import           Control.Monad.Freer.Extended  (Eff, Member, send, runNat)
import           Data.Text (Text)
import Data.List (find)
import Mailchimp

type Subscriber = Text

data SubscriberList a where
  FetchListId      :: Text -> SubscriberList (Either String Text)
  FetchListMembers :: Text -> SubscriberList (Either String [Text])
  SubscribeMember  :: Text -> Subscriber -> SubscriberList (Either String ())

deriving instance Show (SubscriberList a)

fetchListId :: (Member SubscriberList r) => Text -> Eff r (Either String Text)
fetchListId  = send . FetchListId

fetchListMembers :: (Member SubscriberList r) => Text -> Eff r (Either String [Text])
fetchListMembers = send . FetchListMembers

subscribeMember :: (Member SubscriberList r) => Text -> Subscriber -> Eff r (Either String ())
subscribeMember listId email = send $ SubscribeMember listId email

runSubscriberList :: (Member IO r) => Config -> Eff (SubscriberList ': r) a -> Eff r a
runSubscriberList config = runNat subscriberListToIO
  where
    nameMatches :: Text -> MailchimpSimpleSingleList -> Bool
    nameMatches listName (MailchimpSimpleSingleList (name, _)) = name == listName

    subscriberListToIO :: SubscriberList a -> IO a
    subscriberListToIO (FetchListId listName) = do
      listsResponse <- runMailchimp config fetchListsClient
      case listsResponse of
        Left err -> return $ Left (show err)
        Right (MailchimpListResponse lists) ->
          case find (nameMatches listName) lists of
            Nothing -> return $ Left "Could not find list with that name!"
            Just (MailchimpSimpleSingleList (_, id_)) -> return $ Right id_

    subscriberListToIO (FetchListMembers listId) = do
      membersResponse <- runMailchimp config (\auth -> fetchSubscribersClient auth listId (Just 2000))
      case membersResponse of
        Left err -> return $ Left (show err)
        Right (MailchimpMembersResponse subs) -> return $ Right $ map unMailchimpSubscriber subs

    subscriberListToIO (SubscribeMember listId email) = do
      subscribeResponse <- runMailchimp config (\auth -> subscribeNewUserClient auth listId (MailchimpSubscriber email))
      case subscribeResponse of
        Left err -> return $ Left (show err)
        Right _  -> return $ Right ()
