{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.MakeAuthor where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { login :: String
    , token :: String
    , descr :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
