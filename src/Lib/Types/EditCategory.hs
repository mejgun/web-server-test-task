{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.EditCategory where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { cat_id :: Int
    , name :: String
    , parent :: Maybe Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
