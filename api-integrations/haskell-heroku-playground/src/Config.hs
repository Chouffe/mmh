module Config
( Environment (..)
)
where

data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)
