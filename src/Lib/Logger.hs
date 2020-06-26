module Lib.Logger
  ( Handle(..)
  , LogLevel(..)
  , Logger
  )
where

data LogLevel
  = LogDebug
  | LogNormal
  | LogQuiet
  deriving (Eq, Ord)

type Logger = LogLevel -> String -> IO ()

newtype Handle =
  Handle
    { logg :: Logger
    }
