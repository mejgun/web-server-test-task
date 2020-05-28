{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.CreateUser
  ( createUser
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    , login    :: String
    , password :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

createUser :: MyHandler Req
createUser conn respond u = handleSqlErr respond $ do
  _ <- execute
    conn
    "insert into users (name,lastname,photo,token,login,password) values(?,?,?,md5(random()::text),?,md5(?)) on conflict do nothing;"
    (name u, lastname u, photo u, login u, password u)
  respond responseOK
