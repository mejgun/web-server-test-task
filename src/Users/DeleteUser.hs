{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.DeleteUser
  ( deleteUser
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

deleteUser :: MyHandler Req
deleteUser conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute conn "delete from users where login=?;" [login u]
    respond responseOK

