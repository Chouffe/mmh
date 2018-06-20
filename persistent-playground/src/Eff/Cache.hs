{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Eff.Cache where

import           Control.Monad         (void)
import           Data.ByteString.Char8 (pack, unpack)
import           Database.Redis        (get, setex, del, runRedis, connect, Redis)
import           Control.Monad.Freer   (LastMember, Member, Eff, send, interpretM)
import           Control.Natural
import           Data.Int              (Int64)

import           Cache                 (RedisInfo)
import           Eff.Utils             (runNat)
import           Schema                (User)


data Cache a where
  CacheUser        :: Int64 -> User -> Cache ()
  FetchCachedUser  :: Int64 -> Cache (Maybe User)
  DeleteCachedUser :: Int64 -> Cache ()

cacheUser :: (Member Cache r) => Int64 -> User -> Eff r ()
cacheUser uid user = send $ CacheUser uid user

fetchCachedUser :: (Member Cache r) => Int64 -> Eff r (Maybe User)
fetchCachedUser uid = send $ FetchCachedUser uid

deleteCachedUser :: (Member Cache r) => Int64 -> Eff r ()
deleteCachedUser uid = send $ DeleteCachedUser uid

runRedisAction :: (Member IO r) => RedisInfo -> Eff (Redis ': r) a -> Eff r a
runRedisAction redisInfo = runNat redisToIO
  where
    redisToIO :: Redis a -> IO a
    redisToIO action = do
      connection <- connect redisInfo
      runRedis connection action

runRedisAction' :: (LastMember IO r) => RedisInfo -> Eff (Redis ': r) ~> Eff r
runRedisAction' redisInfo = interpretM redisToIO
  where
    redisToIO :: Redis a -> IO a
    redisToIO action = do
      connection <- connect redisInfo
      runRedis connection action


runCache :: (Member Redis r) => Eff (Cache ': r) a ->  Eff r a
runCache = runNat cacheToRedis
  where
    cacheToRedis :: Cache a -> Redis a
    cacheToRedis (CacheUser uid user) = void $ setex (pack . show $ uid) 3600 (pack . show $ user)
    cacheToRedis (FetchCachedUser uid) = do
      result <- get (pack . show $ uid)
      case result of
        Right (Just userString) -> return $ Just (read . unpack $ userString)
        _                       -> return Nothing
    cacheToRedis (DeleteCachedUser uid) = void $ del [(pack . show $ uid)]
