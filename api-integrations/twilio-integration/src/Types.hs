{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( IncomingMessage (..)
  , Environment (..)
  )
  where

import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)
import           Web.FormUrlEncoded  (Form (..), FromForm (..))

data IncomingMessage
  = IncomingMessage
      { fromNumber :: Text
      , body       :: Text
      }
  deriving Show


instance FromForm IncomingMessage where
  fromForm :: Form -> Either Text IncomingMessage
  fromForm (Form form) =
    case lookupResults of
      Just ((fromNum : _), (bdy : _)) ->
        Right $ IncomingMessage fromNum bdy

      Just _ ->
        Left "Found the keys but no values"

      Nothing ->
        Left "Didn’t find keys"

    where
      lookupResults :: Maybe ([Text], [Text])
      lookupResults = do
        fromNum <- HM.lookup "From" form
        bdy     <- HM.lookup "Body" form
        return (fromNum, bdy)

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)
