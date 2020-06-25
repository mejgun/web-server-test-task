module Lib.Logger
  ( Handle(..)
  , LogLevel(..)
  , Logger
  , logDebug
  , logNormal
  , logQuiet
  )
where

data LogLevel = LogDebug
    | LogNormal
    | LogQuiet
    deriving (Eq, Ord)

-- type AppLogger = Handle -> LogLevel -> LogLevel -> String -> IO ()

type Logger = LogLevel -> String -> IO ()

newtype Handle = Handle {logg::Logger}

logDebug, logNormal, logQuiet :: Handle -> String -> IO ()
logDebug = (`logg` LogDebug)
logNormal = (`logg` LogNormal)
logQuiet = (`logg` LogQuiet)
