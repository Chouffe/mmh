{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Eff.App
  ( runAppEff
  , AppEff
  , transformEffToHandler
  ) where

import           Control.Monad.Freer    (Eff, runM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (LoggingT)
import           Data.Either            (either)
import           Database.Persist.Sql   (SqlPersistT)
import           Database.Redis         (Redis)
import           Errors                 (runWithServantHandler)
import           Servant                (throwError)
import           Servant.Server         ((:~>) (..), Handler (..))

import           Cache                  (RedisInfo)
import           Database               (SQLiteInfo)
import           Eff.Cache              (Cache, runCache, runRedisAction)
import           Eff.Database           (Database, runDatabase, runSqlPersist)


type AppEff = Eff '[Cache, Redis, Database, SqlPersistT (LoggingT IO), IO]

runAppEff :: SQLiteInfo -> RedisInfo -> AppEff a -> IO a
runAppEff sqliteInfo redisInfo eff = runM
  $ runSqlPersist sqliteInfo
  $ runDatabase
  $ runRedisAction redisInfo
  $ runCache eff

transformEffToHandler :: SQLiteInfo -> RedisInfo -> AppEff :~> Handler
transformEffToHandler sqliteInfo redisInfo = NT $ \action -> do
  result <- liftIO
    $ runWithServantHandler
    $ runAppEff sqliteInfo redisInfo action
  Handler $ either throwError return result
