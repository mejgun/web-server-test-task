{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.CreateTag where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { name :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
