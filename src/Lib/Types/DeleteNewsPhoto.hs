{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteNewsPhoto where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { photo_id :: Int
    , news_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
