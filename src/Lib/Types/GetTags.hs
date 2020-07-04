{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetTags where

import qualified Data.Aeson                    as A
                                                ( FromJSON
                                                , ToJSON
                                                )
import           Database.PostgreSQL.Simple     ( FromRow )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { page :: Int
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Tag =
  Tag
    { id :: Int
    , name :: String
    }
  deriving (Generic, Show, Eq)

instance FromRow Tag

instance A.ToJSON Tag
