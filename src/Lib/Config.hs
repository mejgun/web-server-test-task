{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Config
  ( Lib.Config.read
  , Config(..)
  )
where

import           Data.Aeson                    as A
import qualified Data.ByteString.Char8         as B8
import           Data.Maybe                     ( fromMaybe )
import           Database.PostgreSQL.Simple     ( Connection
                                                , connectPostgreSQL
                                                )
import           GHC.Generics

import qualified Lib.Logger                    as Logger

data Conf =
  Conf
    { pgconfig :: String
    , log_level :: String
    , log_file :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Conf

data Config =
  Config
    { connection :: Connection
    , logFile :: String
    , logLevel :: Logger.LogLevel
    }

read :: FilePath -> IO Config
read f = do
  conf <-
    fromMaybe (error "ERROR: Bad config") <$> A.decodeFileStrict f :: IO Conf
  conn <- connectPostgreSQL $ B8.pack $ pgconfig conf
  let lgLvl = strToLogLevel $ log_level conf
  return Config { connection = conn, logFile = log_file conf, logLevel = lgLvl }
 where
  strToLogLevel :: String -> Logger.LogLevel
  strToLogLevel s = case s of
    "quiet"  -> Logger.LogQuiet
    "normal" -> Logger.LogNormal
    _        -> Logger.LogDebug
