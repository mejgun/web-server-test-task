{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.AddNewsTag where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { news_id :: Int
    , tag_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
