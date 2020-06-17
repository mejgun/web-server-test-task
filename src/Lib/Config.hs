{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Config
  ( readConfig
  , module Database.PostgreSQL.Simple
  , Config(..)
  )
where

import qualified Data.Aeson                    as A
import qualified Data.ByteString.Char8         as B8
import           Data.Maybe                     ( fromJust )
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Conf = Conf
    { pgconfig  :: String
    , log_level :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Conf

data Config = Config
    { connection :: Connection
    }

readConfig :: IO Config
readConfig = do
  j <- fromJust <$> A.decodeFileStrict "config.json" :: IO Conf
  c <- connectPostgreSQL $ B8.pack $ pgconfig j
  return Config { connection = c }
