{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.LoginUser where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           GHC.Generics

data Request =
  Request
    { login :: String
    , password :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Token =
  Token
    { token :: String
    }
  deriving (Generic, Show, Eq)

instance FromRow Token

instance A.ToJSON Token
