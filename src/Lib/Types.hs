{-# LANGUAGE DeriveGeneric #-}

module Lib.Types where

import           Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.Wai

import qualified Lib.Logger                    as Logger
import qualified Lib.Logic                     as Logic

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
