{-# LANGUAGE OverloadedStrings #-}

module Stub.Article where

import           Data.Text            (Text)
import           Data.Time            (getCurrentTime)
import           Database.Persist.Sql (Key)

import           Schema               (Article (..), User (..))

mkSampleArticle :: Text -> Key User -> IO Article
mkSampleArticle title key = do
  now <- getCurrentTime
  return $ Article title "Lorem Ipsum Body" now key
