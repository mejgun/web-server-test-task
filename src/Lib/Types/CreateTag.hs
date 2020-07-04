{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.CreateTag where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { name :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
