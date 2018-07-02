{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Logger
  ( Config (..)
  , fetchConfig
  , Handle (..)
  , withHandle

  , Verbosity (..)
  , debug
  , info
  , warning
  , error
  )
  where

import           Control.Exception     (bracket)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Time.Clock       (getCurrentTime)
import           Prelude               hiding (error)
import qualified System.Log.FastLogger as FL

import           Internal              (Environment (..))

data Verbosity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

data Config
  = Config
    { cPath      :: Maybe FilePath
    , cVerbosity :: Maybe Verbosity
    }
    deriving (Show, Eq)

fetchConfig :: Environment -> IO Config
fetchConfig Production = return $ Config
    { cVerbosity = Just Info
    , cPath = Just "web-app.prod.log"
    }
fetchConfig Development = return $ Config
    { cVerbosity = Just Debug
    , cPath = Nothing
    }
fetchConfig Test = return $ Config
    { cVerbosity = Just Warning
    , cPath = Nothing
    }

data Handle = Handle
  { hConfig    :: Config
  , hLoggerSet :: FL.LoggerSet
  }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config@Config{..} f = bracket
  (case cPath of
     Nothing   -> FL.newStderrLoggerSet FL.defaultBufSize
     Just "-"  -> FL.newStderrLoggerSet FL.defaultBufSize
     Just path -> FL.newFileLoggerSet FL.defaultBufSize path)
  FL.rmLoggerSet
  (\l -> f Handle { hConfig = config, hLoggerSet = l })

log' :: FL.ToLogStr s => Handle -> Verbosity -> s -> IO ()
log' Handle {..} v x
  | v >= verbosity = do
    t <- getCurrentTime
    FL.pushLogStrLn hLoggerSet $ FL.toLogStr ("[" <> (show t) <> "]") <> FL.toLogStr x
  | otherwise = return ()

  where
    verbosity :: Verbosity
    verbosity = fromMaybe Debug (cVerbosity hConfig)

debug, info, warning, error ::  FL.ToLogStr s => Handle -> s -> IO ()
debug h   = log' h Debug
info h    = log' h Info
warning h = log' h Warning
error h   = log' h Error
