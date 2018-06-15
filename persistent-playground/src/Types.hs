{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import           Data.Aeson       (FromJSON, Object, ToJSON, object, parseJSON,
                                   toJSON, withObject, (.:), (.=))
import           Data.Aeson.Types (Parser)
import           Data.Int         (Int64)


newtype KeyVal a = KeyVal (Int64, a)

instance Show a => Show (KeyVal a) where
  show (KeyVal (k, v)) = "KeyVal (" ++ show k ++ "," ++ show v ++ ")"

getKey :: KeyVal a -> Int64
getKey (KeyVal (k, _)) = k

getVal :: KeyVal a -> a
getVal (KeyVal (_, v)) = v


instance ToJSON a => ToJSON (KeyVal a) where
  toJSON (KeyVal (k, v)) = object $
    [ "key" .= k
    , "val" .= v
    ]

instance FromJSON a => FromJSON (KeyVal a) where
  parseJSON = withObject "Key Val Item" parseKeyVal
    where
      parseKeyVal :: Object -> Parser (KeyVal a)
      parseKeyVal o = do
        key       <- o .: "key"
        val       <- o .: "val"
        return $ KeyVal (key, val)

data ServerMode
  = Prod
  | Dev
  | Test
  deriving (Eq, Show)
