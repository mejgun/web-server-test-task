{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteCategory where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { cat_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
