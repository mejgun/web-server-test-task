{-# LANGUAGE OverloadedStrings #-}

module Lib.Config where

import           Lib.Types

read :: FilePath -> IO Config
read f = do
  j <- fromMaybe (error "ERROR: Bad config") <$> A.decodeFileStrict f :: IO Conf
  c <- connectPostgreSQL $ B8.pack $ pgconfig j
  h <- openFile (log_file j) AppendMode
  let lgLvl = strToLogLevel $ log_level j
  return Config { connection = c
                , hnd        = h
                , logger     = logg h lgLvl
                , loglevel   = lgLvl
                }
 where
  logg :: AppLogger
  logg h appLogLvl msgLogLvl s =
    if appLogLvl <= msgLogLvl then hPutStrLn h s >> hFlush h else return ()

  strToLogLevel :: String -> LogLevel
  strToLogLevel s = case s of
    "quiet"  -> LogQuiet
    "normal" -> LogNormal
    _        -> LogDebug
