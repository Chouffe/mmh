module Types where

import           Data.Int (Int64)


newtype KeyVal a = KeyVal (Int64, a)

data ServerMode
  = Prod
  | Dev
  | Test
  deriving (Eq, Show)
