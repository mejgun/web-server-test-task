{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.CreateNews where

import qualified Data.Aeson                    as A
                                                ( FromJSON
                                                , ToJSON
                                                )
import           Database.PostgreSQL.Simple     ( FromRow )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { name :: String
    , cat_id :: Int
    , text :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data NewsId =
  NewsId
    { news_id :: Int
    }
  deriving (Generic, Show, Eq)

instance FromRow NewsId

instance A.ToJSON NewsId
