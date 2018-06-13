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
import           Data.Time            (getCurrentTime)
import           Data.Time.Clock      (UTCTime (..))
import           Database.Persist     (Entity (..), Key (..))
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
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

    Article sql=articles
      title Text
      body Text
      publishedTime UTCTime
      authorId UserId
      UniqueTitle title
      deriving Show Read Eq
  |]

parseUser :: Object -> Parser User
parseUser o = do
  uName       <- o .: "name"
  uEmail      <- o .: "email"
  uAge        <- o .: "age"
  uOccupation <- o .: "occupation"
  return $ User uName uEmail uAge uOccupation

parseArticle :: Object -> Parser Article
parseArticle o = do
  aTitle         <- o .: "title"
  aBody          <- o .: "body"
  aPublishedTime <- o .: "publishedTime"
  aAuthorId      <- o .: "authorId"
  return $ Article aTitle aBody aPublishedTime aAuthorId

instance FromJSON User where
  parseJSON = withObject "User" parseUser

instance ToJSON (Entity User) where
  toJSON (Entity uid user) =
    object $ "id" .= (fromSqlKey uid) : userPairs user

instance ToJSON User where
  toJSON user = object (userPairs user)

userPairs :: User -> [Pair]
userPairs user =
  [ "name" .= userName user
  , "email" .= userEmail user
  , "age" .= userAge user
  , "occupation" .= userOccupation user
  ]

instance FromJSON (Entity User) where
  parseJSON = withObject "User Entity" $ \o -> do
    user <- parseUser o
    uid <- o .: "id"
    return $ Entity (toSqlKey uid) user

instance ToJSON (Entity Article) where
  toJSON (Entity aid article) = object $ "id" .= (fromSqlKey aid) : articlePairs article

instance ToJSON Article where
  toJSON article = object (articlePairs article)

articlePairs :: Article -> [Pair]
articlePairs article =
  [ "title" .= articleTitle article
  , "body" .= articleBody article
  , "publishedTime" .= articlePublishedTime article
  , "authorId" .= fromSqlKey (articleAuthorId article)
  ]

instance FromJSON (Entity Article) where
  parseJSON =
    withObject "Article Entity" $ \o -> do
      article <- parseArticle o
      aid <- o .: "id"
      return $ Entity (toSqlKey aid) article

instance FromJSON Article where
  parseJSON = withObject "Article" parseArticle

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

-- Making sample article
mkSampleArticle :: Text -> Key User -> IO Article
mkSampleArticle title key = do
  now <- getCurrentTime
  return $ Article title "Lorem Ipsum Body" now key
