{-# LANGUAGE GADTs        #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cache where

import           Control.Monad         (void)
import           Data.ByteString.Char8 (pack, unpack)
import           Data.Int              (Int64)
import           Database.Redis        (ConnectInfo, Redis, defaultConnectInfo,
                                        del, get, setex)
import           Monad.Cache           (MonadCache (..))
import           Schema
import           Types


type RedisInfo = ConnectInfo

fetchRedisInfo :: ServerMode -> IO RedisInfo
fetchRedisInfo Test = return defaultConnectInfo
fetchRedisInfo Dev  = return defaultConnectInfo
fetchRedisInfo Prod = return defaultConnectInfo


instance MonadCache Redis where

  cacheUser :: Int64 -> User -> Redis ()
  cacheUser uid user = void $ setex (pack . show $ uid) 3600 (pack . show $ user)

  fetchCachedUser  :: Int64 -> Redis (Maybe User)
  fetchCachedUser uid = do
    result <- get (pack . show $ uid)
    case result of
      Right (Just userString) -> return $ Just (read . unpack $ userString)
      _                       -> return Nothing

  deleteCachedUser :: Int64 -> Redis ()
  deleteCachedUser uid = void $ del [(pack . show $ uid)]
