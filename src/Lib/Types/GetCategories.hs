{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetCategories where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { page :: Int
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Cat =
  Cat
    { id :: Int
    , name :: String
    , parent :: Maybe Int
    }
  deriving (Generic, Show, Eq)

instance FromRow Cat

instance A.ToJSON Cat
