{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.LoginUser
  ( loginUser
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { login    :: String
    , password :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

data Token = Token
    { token :: String
    }
    deriving (Generic, Show)

instance FromRow Token
instance A.ToJSON Token

loginUser :: MyHandler Req
loginUser conn respond u = handleSqlErr respond $ do
  t <-
    query conn
          "select token from users where login = ? and password = md5(?);"
          [login u, password u] :: IO [Token]
  if null t then respond responseERR else respond $ respJSON $ head t

