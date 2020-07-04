{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.CreateCategory where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { name :: String
    , parent :: Maybe Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
