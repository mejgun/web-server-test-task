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

deleteUser :: MyApp
deleteUser conn req respond = do
  p <- bodyToJSON req :: IO (Maybe DeleteUser)
  maybe
    (respond responseERR)
    (\u -> do
      adm <- isAdmin conn $ token u
      if adm
        then handle (checkSqlErr (respond responseSQLERR)) $ do
          _ <- execute conn "delete from users where login=?;" [login u]
          respond responseOK
        else respond responseERR
    )
    p
