{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.LoginUser
  ( logIn
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

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

logIn :: MyHandler Req Token
logIn conn _ u = do
  t <-
    query conn
          "select token from users where login=? and password=md5(?);"
          [login u, password u] :: IO [Token]
  if null t then throw ErrBadRequest else return $ head t
