{-# LANGUAGE DeriveGeneric #-}

module Lib.Requests.CreateUser where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { name :: String
    , lastname :: String
    , photo :: Maybe String
    , photo_type :: Maybe String
    , login :: String
    , password :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
