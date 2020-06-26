{-# LANGUAGE OverloadedStrings #-}

module Lib.Config where

import           Data.Aeson                    as A
import qualified Data.ByteString.Char8         as B8
import           Data.Maybe                     ( fromMaybe )
import           Database.PostgreSQL.Simple

import           Lib
import qualified Lib.Logger                    as Logger

read :: FilePath -> IO Config
read f = do
  j <- fromMaybe (error "ERROR: Bad config") <$> A.decodeFileStrict f :: IO Conf
  c <- connectPostgreSQL $ B8.pack $ pgconfig j
  let lgLvl = strToLogLevel $ log_level j
  return Config { connection = c, logFile = log_file j, logLevel = lgLvl }
 where
  strToLogLevel :: String -> Logger.LogLevel
  strToLogLevel s = case s of
    "quiet"  -> Logger.LogQuiet
    "normal" -> Logger.LogNormal
    _        -> Logger.LogDebug
