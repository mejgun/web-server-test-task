{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.EditAuthor where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { login :: String
    , token :: String
    , descr :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
