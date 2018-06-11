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


import           Data.Aeson           (FromJSON, Object, ToJSON, object,
                                       parseJSON, toJSON, withObject, (.:),
                                       (.=))
import           Data.Aeson.Types     (Pair, Parser)
import           Data.Text            (Text)
import           Database.Persist     (Entity (..))
import           Database.Persist.Sql (toSqlKey)
import qualified Database.Persist.TH  as PTH

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
    User sql=users
      name Text
      email Text
      age Int
      occupation Text
      UniqueEmail email
      deriving Show Read
  |]

instance ToJSON User where
  toJSON user = object
    [ "name" .= userName user
    , "email" .= userEmail user
    , "age" .= userAge user
    , "occupation" .= userOccupation user
    ]

instance FromJSON User where
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser o = do
  uName       <- o .: "name"
  uEmail      <- o .: "email"
  uAge        <-  o .: "age"
  uOccupation <- o .: "occupation"
  return $ User uName uEmail uAge uOccupation

-- An entity is just a wrapper around a database row + sql id key
sampleUser :: Entity User
sampleUser = Entity (toSqlKey 1) $ User
  { userName = "admin"
  , userEmail = "admin@test.com"
  , userAge = 23
  , userOccupation = "System Administrator"
  }

-- Fake users
meUser :: User
meUser =  User
  { userName = "chouffe2"
  , userEmail = "chouffe2.caillau@gmail.com"
  , userAge = 28
  , userOccupation = "SWE"
  }

john :: User
john = User "john Doe" "john.doe@gmail.com" 39 "SWE"

jane :: User
jane = User "Jane Dop" "jane.dop@gmail.com" 23 "Teacher"
