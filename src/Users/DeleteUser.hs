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

data DeleteUser = DeleteUser
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON DeleteUser

deleteUser :: MyHandler DeleteUser
deleteUser conn respond u =
  rIfAdmin conn respond (token u)
    $ handle (checkSqlErr (respond responseSQLERR))
    $ do
        _ <- execute conn "delete from users where login=?;" [login u]
        respond responseOK

