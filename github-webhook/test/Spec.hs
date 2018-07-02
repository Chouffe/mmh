{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Aeson           (decode)
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe           (isJust)
import           Test.Hspec

import           API


main :: IO ()
main = do
  hspec
    $ before beforeHook1
    $ spec1

beforeHook1 :: IO (Maybe GithubRequest)
beforeHook1 = do
  payload <- BS.readFile "test/payload/pull_request.json"
  return $ decode payload

  where
    payloadFilename :: FilePath
    payloadFilename = "test/payload/pull_request.json"


spec1 :: SpecWith (Maybe GithubRequest)
spec1 = describe "Parsing a pull request payload" $ do
  it "parses properly" (\ mrq -> isJust mrq `shouldBe` True)
  it "parses as a Pull Request" (\ mrq -> case mrq of
                                            (Just (GithubOpenPRequest _ _)) -> True `shouldBe` True
                                            _ -> error "Failed..."
                                )
  it "parses the proper handlename" (\ mrq -> case mrq of
                                            (Just (GithubOpenPRequest handleName _)) -> handleName `shouldBe` "usernamehandle"
                                            _ -> error "Failed..."
                                )
  it "parses the proper commentsURL" (\ mrq -> case mrq of
                                            (Just (GithubOpenPRequest _ commentsURL)) -> commentsURL `shouldBe` "https://github.com/comments_url"
                                            _ -> error "Failed..."
                                      )
