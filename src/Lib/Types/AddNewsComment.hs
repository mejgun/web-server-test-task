{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.AddNewsComment where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { news_id :: Int
    , token :: String
    , text :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
