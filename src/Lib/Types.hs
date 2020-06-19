{-# LANGUAGE DeriveGeneric #-}

module Lib.Types where

import           Control.Monad.Except
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

type HandlerMonad = ExceptT ResultResponseError IO

type MyHandler a b = Connection -> Logger -> a -> HandlerMonad b

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
    deriving Show

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
