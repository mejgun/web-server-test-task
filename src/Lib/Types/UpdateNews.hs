{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.UpdateNews where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { news_id :: Int
    , name :: String
    , cat_id :: Int
    , text :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
