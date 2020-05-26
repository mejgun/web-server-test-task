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

data LoginUser = LoginUser
    { login    :: String
    , password :: String
    }
    deriving (Generic, Show)

instance A.FromJSON LoginUser

data Token = Token
    { token :: String
    }
    deriving (Generic, Show)

instance FromRow Token
instance A.ToJSON Token

loginUser :: MyApp
loginUser conn req respond = do
  p <- bodyToJSON req :: IO (Maybe LoginUser)
  maybe
    (respond responseERR)
    (\u -> handle (checkSqlErr (respond responseSQLERR)) $ do
      t <-
        query conn
              "select token from users where login = ? and password = md5(?);"
              [login u, password u] :: IO [Token]
      if null t then respond responseERR else respond $ responseJSON $ head t
    )
    p

