{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteTag where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { tag_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
