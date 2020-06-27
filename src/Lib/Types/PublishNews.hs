{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.PublishNews where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { news_id :: Int
    , token :: String
    , publish :: Bool
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data User =
  User
    { name :: String
    , lastname :: String
    , photo :: Maybe String
    }
  deriving (Generic, Show)

instance A.ToJSON User

instance FromRow User
