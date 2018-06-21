{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server where


import           Control.Monad.IO.Class   (liftIO)
import qualified Data.HashMap.Strict      as HM
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.API              ((:<|>), (:>), FormUrlEncoded, Get,
                                           JSON, Post, ReqBody)
import           Servant.Server           (Handler, hoistServer)
import           System.Environment       (getEnv)
import           Twilio
import qualified Twilio.Messages          as TM
import           Web.FormUrlEncoded       (Form (..), FromForm (..))
