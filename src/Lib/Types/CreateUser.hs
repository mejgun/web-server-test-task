{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.CreateUser where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

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
