{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteNewsComment where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { comment_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
