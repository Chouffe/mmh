module Main where

import           API    (runServer)
import           Config (Environment (..))

main :: IO ()
main = runServer Production
