{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetTags where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { page :: Int
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Tag =
  Tag
    { id :: Int
    , name :: String
    }
  deriving (Generic, Show)

instance FromRow Tag

instance A.ToJSON Tag
