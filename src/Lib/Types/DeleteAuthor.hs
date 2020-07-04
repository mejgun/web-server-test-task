{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.DeleteAuthor where

import qualified Data.Aeson                    as A
                                                ( FromJSON )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { login :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request
