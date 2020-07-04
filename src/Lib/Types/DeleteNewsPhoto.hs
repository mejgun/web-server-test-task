{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteNewsPhoto where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { photo_id :: Int
    , news_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
