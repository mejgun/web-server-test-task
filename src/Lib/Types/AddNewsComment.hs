{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.AddNewsComment where

import qualified Data.Aeson                    as A
import           GHC.Generics

data Request =
  Request
    { news_id :: Int
    , token :: String
    , text :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
