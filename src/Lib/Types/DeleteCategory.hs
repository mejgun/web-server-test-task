{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteCategory where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { cat_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
