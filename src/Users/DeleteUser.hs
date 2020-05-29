{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.DeleteUser
  ( deleteUser
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           Control.Monad                  ( when )
import           System.Directory               ( removeFile )

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
    q <- query conn "delete from users where login=? returning photo;" [login u]
    case q of
      [Only f] -> when (not (null f)) $ removeFile f
      _        -> return ()
    respond responseOK

