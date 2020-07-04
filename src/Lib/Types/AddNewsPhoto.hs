{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.AddNewsPhoto where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { news_id :: Int
    , token :: String
    , photo :: String
    , photo_type :: Maybe String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
