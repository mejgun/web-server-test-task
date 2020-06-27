{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteUser where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { login :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
