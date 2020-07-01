{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.CreateNews where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { name :: String
    , cat_id :: Int
    , text :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data NewsId =
  NewsId
    { news_id :: Int
    }
  deriving (Generic, Show, Eq)

instance FromRow NewsId

instance A.ToJSON NewsId
