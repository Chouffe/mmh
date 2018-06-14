{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import           Data.Aeson       (FromJSON, Object, ToJSON, object, parseJSON,
                                   toJSON, withObject, (.:), (.=))
import           Data.Aeson.Types (Parser)
import           Data.Int         (Int64)
import           Data.Text        (pack)


newtype KeyVal a = KeyVal (Int64, a)

instance ToJSON a => ToJSON (KeyVal a) where
  toJSON (KeyVal (k, v)) = object $
    [ "key" .= (pack . show) k
    , "val" .= toJSON v
    ]

instance FromJSON a => FromJSON (KeyVal a) where
  parseJSON = withObject "KeyVal" parseKeyVal
    where
      parseKeyVal :: Object -> Parser (KeyVal a)
      parseKeyVal o = do
        key       <- o .: "key"
        val       <- o .: "val"
        parsedVal <- parseJSON val
        return $ KeyVal (key, parsedVal)

data ServerMode
  = Prod
  | Dev
  | Test
  deriving (Eq, Show)
