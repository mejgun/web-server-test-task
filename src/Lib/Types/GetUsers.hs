{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetUsers where

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

data User =
  User
    { name :: String
    , lastname :: String
    , photo :: Maybe String
    }
  deriving (Generic, Show, Eq)

instance A.ToJSON User

instance FromRow User
