{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteNewsComment where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { comment_id :: Int
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
