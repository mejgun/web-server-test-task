{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.DeleteUser
  ( deleteUser
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           Network.Wai

import           PG
import           Types

data DeleteUser = DeleteUser
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.ToJSON DeleteUser
instance A.FromJSON DeleteUser

deleteUser :: MyApp
deleteUser conn req respond = do
  b <- lazyRequestBody req
  let p = A.decode b :: Maybe DeleteUser
  case p of
    Just u -> do
      adm <- isAdmin conn $ token u
      if adm
        then do
          _ <- execute conn "delete from users where login=?;" [login u]
          respond responseOK
        else respond responseERR
    _ -> respond responseERR


