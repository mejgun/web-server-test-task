{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetCategories where

import qualified Data.Aeson                    as A
                                                ( FromJSON
                                                , ToJSON
                                                )
import           Database.PostgreSQL.Simple     ( FromRow )
import           GHC.Generics                   ( Generic )

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
