{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteNews where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { news_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
