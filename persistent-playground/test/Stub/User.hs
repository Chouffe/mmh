{-# LANGUAGE OverloadedStrings #-}

module Stub.User where

import           Data.Int             (Int64)
import           Database.Persist.Sql (toSqlKey)

import           Schema


testUser1 :: User
testUser1 = User
  { userName = "james"
  , userEmail = "james@test.com"
  , userAge = 25
  , userOccupation = "Software Engineer"
  }

testUser2 :: User
testUser2 = User
  { userName = "kate"
  , userEmail = "kate@test.com"
  , userAge = 24
  , userOccupation = "Software Engineer"
  }

testUser3 :: User
testUser3 = User
  { userName = "jeremy"
  , userEmail = "jeremy@test.com"
  , userAge = 23
  , userOccupation = "Teacher"
  }

testUser4 :: User
testUser4 = User
  { userName = "alex"
  , userEmail = "alex@test.com"
  , userAge = 30
  , userOccupation = "Petroleum Engineer"
  }

testUser5 :: User
testUser5 = User
  { userName = "adam"
  , userEmail = "adam@test.com"
  , userAge = 30
  , userOccupation = "Accountant"
  }

testUser6 :: User
testUser6 = User
  { userName = "alexa"
  , userEmail = "alexa@test.com"
  , userAge = 30
  , userOccupation = "Mechanical Engineer"
}
