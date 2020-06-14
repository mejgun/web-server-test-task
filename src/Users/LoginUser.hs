{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.LoginUser
  ( log_in
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

log_in :: MyHandler Req Token
log_in conn u = handleSqlErr $ do
  t <-
    query conn
          "select token from users where login=? and password=md5(?);"
          [login u, password u] :: IO [Token]
  return $ if null t then ErrorBadRequest else OkJSON $ head t

