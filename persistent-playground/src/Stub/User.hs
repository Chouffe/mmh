{-# LANGUAGE OverloadedStrings #-}

module Stub.User where

import           Schema (User (..))

meUser :: User
meUser =  User
  { userName = "Blah"
  , userEmail = "test.test@gmail.com"
  , userAge = 53
  , userOccupation = "SWE"
  }

john :: User
john = User "john Doe" "john.doe@gmail.com" 39 "SWE"

jane :: User
jane = User "Jane Dop" "jane.dop@gmail.com" 23 "Teacher"
