{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schema where

import           Data.Aeson          (FromJSON, ToJSON, object, parseJSON,
                                      toJSON, withObject, (.:), (.=))
import           Data.Text           (Text)
import qualified Database.Persist.TH as PTH


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
    Subscriber sql=subscribers
      email Text
      UniqueSubscriberEmail email
      deriving Show Read Eq
  |]

instance ToJSON Subscriber where
  toJSON sub = object
    [ "email" .= subscriberEmail sub
    ]

instance FromJSON Subscriber where
  parseJSON = withObject "Subscriber" $ \o -> do
    sEmail <- o .: "email"
    return Subscriber { subscriberEmail = sEmail }
