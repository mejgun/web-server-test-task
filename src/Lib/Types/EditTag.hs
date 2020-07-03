{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.EditTag where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { tag_id :: Int
    , name :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
