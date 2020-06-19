{-# LANGUAGE DeriveGeneric #-}

module Lib.Types where

import           Control.Exception
import           Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.Wai
import           System.IO                      ( Handle )

type MyApp
  =  Connection
  -> Logger
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived

type MyHandler a b = Connection -> Logger -> a -> IO b

data ResultResponseError = Error404
    | ErrorBadRequest
    | ErrorNotAuthor
    | ErrorLoginNotExist
    | ErrorLoginAlreadyExist
    | ErrorBadPage
    | ErrorCategoryNotExist
    | ErrorTagAlreadyExist
    | ErrorTagNotExist
    | ErrorNewsNotExist
    | ErrorNotYourNews
    | ErrorNotUser
    | ErrorAuthorNotExist
    deriving Show

instance Exception ResultResponseError

data LogLevel = LogDebug
    | LogNormal
    | LogQuiet
    deriving (Eq, Ord)

type AppLogger = Handle -> LogLevel -> LogLevel -> String -> IO ()

type Logger = LogLevel -> String -> IO ()

data Conf = Conf
    { pgconfig  :: String
    , log_level :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Conf

data Config = Config
    { connection :: Connection
    , hnd        :: Handle
    , logger     :: Logger
    , loglevel   :: LogLevel
    }
