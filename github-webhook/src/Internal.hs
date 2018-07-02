module Internal
  (Environment (..))
  where

data Environment
  = Production
  | Development
  | Test
  deriving (Eq, Show, Read)
