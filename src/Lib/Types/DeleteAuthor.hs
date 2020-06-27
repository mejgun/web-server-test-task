{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteAuthor where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { login :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
