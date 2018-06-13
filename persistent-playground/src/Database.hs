{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Logger        (LoggingT, runStdoutLoggingT)
import           Control.Monad.Reader        (runReaderT)
import           Data.ByteString.Char8       (pack, unpack)
import           Data.Maybe                  (listToMaybe)
import           Data.Text                   (Text)
import           Database.Esqueleto          (InnerJoin (..), desc, from, limit,
                                              on, orderBy, select, val, where_,
                                              (<.), (==.), (^.))
import qualified Database.Esqueleto          as E
import           Database.Persist            (Entity, delete, entityVal, get,
                                              insert)
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              runMigration, withPostgresqlConn)
import           Database.Persist.Sql        (fromSqlKey, runSqlConn, toSqlKey)
import           Database.Persist.Sqlite     (runSqlite, withSqliteConn)
import           Database.Redis              (ConnectInfo, Redis, connect,
                                              defaultConnectInfo, runRedis,
                                              setex)
import qualified Database.Redis              as Redis
import           GHC.Int

import           Schema


-- Config
type RedisInfo = ConnectInfo
type SQLiteInfo = Text
type PGInfo = ConnectionString

data ServerMode
  = Prod
  | Dev
  | Test
  deriving (Eq, Show)


-- Config fetching
fetchRedisInfo :: ServerMode -> IO RedisInfo
fetchRedisInfo Test = return defaultConnectInfo
fetchRedisInfo Dev  = return defaultConnectInfo
fetchRedisInfo Prod = return defaultConnectInfo

fetchSQLiteInfo :: ServerMode -> IO SQLiteInfo
fetchSQLiteInfo Test = return "test.sqlite3"
fetchSQLiteInfo Dev  = return "dev.sqlite3"
fetchSQLiteInfo Prod = return "prod.sqlite3"

fetchPGInfo :: IO PGInfo
fetchPGInfo = return connString

-- Postgresql Backend
connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=password"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action =
  runStdoutLoggingT
    $ withPostgresqlConn connectionString
    $ \backend -> runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)

-- Sqlite backend
connStringSqlite :: Text
connStringSqlite = "dev.sqlite3"

migrateSqliteDB :: IO ()
migrateSqliteDB = runSqlite connStringSqlite (runMigration migrateAll)

runSqliteAction :: Text -> SqlPersistT (LoggingT IO) a -> IO a
runSqliteAction connectionString query =
  runStdoutLoggingT
    $ withSqliteConn connectionString
    $ \backend -> runSqlConn query backend

-- DB Queries
insertUser :: MonadIO m => User -> SqlPersistT m (Key User)
insertUser user = insert user

selectYoungUsers :: MonadIO m => SqlPersistT m [Entity User]
selectYoungUsers = select . from $ \users -> do
  where_ (users ^. UserAge <. val 30)
  return users

  -- Or with Persistent
  -- selectList [UserAge <. 30, UserOccupation ==. "Software Engineer"] []

selectYoungUsers' :: MonadIO m => SqlPersistT m [Entity User]
selectYoungUsers' = select . from $ \users -> do
  where_ (users ^. UserAge <. val 30)
  orderBy [desc (users ^. UserEmail)]
  limit 100
  return users

  -- Or with Persistent
  -- selectList [UserAge <. 30, UserOccupation ==. "Software Engineer"] [Asc UserEmail, LimitTo 100]

selectUserByKey :: MonadIO m => Int -> SqlPersistT m (Maybe User)
selectUserByKey userId = get
  $ toSqlKey
  $ fromIntegral userId


-- SQLite CRUD Queries
fetchUserSQLite :: Text -> Int64 -> IO (Maybe User)
fetchUserSQLite connStr userId = runSqliteAction connStr $ get (toSqlKey userId)

deleteUserSQLite :: Text -> Int64 -> IO ()
deleteUserSQLite connStr userId = runSqliteAction connStr $ delete ((toSqlKey userId) :: Key User)

createUserSQLite :: Text -> User -> IO Int64
createUserSQLite connStr user =
  fromSqlKey
    <$> runSqliteAction connStr (insert user)

getAllUsersSQLite :: Text -> IO [User]
getAllUsersSQLite connStr = do
  entities <- runSqliteAction connStr $ selectYoungUsers'
  return $ fmap entityVal entities

createArticleSQLite :: SQLiteInfo -> Article -> IO Int64
createArticleSQLite connStr article =
  fromSqlKey
    <$> runSqliteAction connStr (insert article)

deleteArticleSQLite :: SQLiteInfo -> Int64 -> IO ()
deleteArticleSQLite connStr articleid = runSqliteAction connStr $ delete ((toSqlKey articleid) :: Key Article)

fetchArticleSQLite :: SQLiteInfo -> Int64 -> IO (Maybe Article)
fetchArticleSQLite connStr articleid = runSqliteAction connStr selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe Article)
    selectAction = ((fmap entityVal ) . listToMaybe)
      <$> (E.select . E.from $ \articles -> do
            E.where_ (articles E.^. ArticleId E.==. E.val (toSqlKey articleid))
            return articles)

fetchArticleSQLite2 :: SQLiteInfo -> Int64 -> IO (Maybe (Entity Article))
fetchArticleSQLite2 connStr aid = runSqliteAction connStr selectAction
  where
    selectAction :: SqlPersistT (LoggingT IO) (Maybe (Entity Article))
    selectAction = listToMaybe
      <$> (select . from $ \articles -> do
            where_ (articles ^. ArticleId ==. val (toSqlKey aid))
            return articles)

fetchArticleByAuthorSQLite :: SQLiteInfo -> Int64 -> IO [Entity Article]
fetchArticleByAuthorSQLite connStr uid = runSqliteAction connStr fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Article]
    fetchAction = select . from $ \articles -> do
      where_ (articles ^. ArticleAuthorId ==. val (toSqlKey uid))
      return articles

fetchRecentArticlesSQLite :: SQLiteInfo -> Int -> IO [(Entity User, Entity Article)]
fetchRecentArticlesSQLite connStr k = runSqliteAction connStr fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity User, Entity Article)]
    fetchAction = select . from $ \(users `InnerJoin` articles) -> do
      on (users ^. UserId ==. articles ^. ArticleAuthorId)
      orderBy [desc (articles ^. ArticlePublishedTime)]
      limit (fromIntegral k)
      return (users, articles)

-- Redis Caching
runRedisAction :: RedisInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
  connection <- connect redisInfo
  runRedis connection action

cacheUser :: RedisInfo -> Int64 -> User -> IO ()
cacheUser redisInfo userid user =
  runRedisAction redisInfo
    $ void
    $ setex (pack . show $ userid) 3600 (pack . show $ user)

deleteUserCache :: RedisInfo -> Int64 -> IO ()
deleteUserCache redisInfo userid =
  runRedisAction redisInfo
    $ void
    $ Redis.del [(pack . show $ userid)]

fetchUserRedis :: RedisInfo -> Int64 -> IO (Maybe User)
fetchUserRedis redisInfo userid = runRedisAction redisInfo $ do
    result <- Redis.get (pack . show $ userid)
    case result of
      Right (Just userString) -> return $ Just (read . unpack $ userString)
      _                       -> return Nothing
