{-# LANGUAGE DeriveGeneric #-}

module Lib.Types where

import           Control.Exception
import           Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Network.Wai

import qualified Lib.Logger                    as Logger

type MyApp
  =  Connection
  -> Logger.Logger
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived

type MyHandler a b = Connection -> Logger.Logger -> a -> IO b

data ResultResponseError = ErrorNotFound
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
    | ErrorInternal
    deriving Show

instance Exception ResultResponseError

data Conf = Conf
    { pgconfig  :: String
    , log_level :: String
    , log_file  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Conf

data Config = Config
    { connection :: Connection
    , logFile    :: String
    , logLevel   :: Logger.LogLevel
    }
