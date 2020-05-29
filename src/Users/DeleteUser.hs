{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.DeleteUser
  ( deleteUser
  )
where

import           Control.Monad                  ( when )
import qualified Data.Aeson                    as A
import           GHC.Generics
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
deleteUser conn u = rIfAdmin conn (token u) $ handleSqlErr $ do
  q <-
    query conn "delete from users where login=? returning photo;" [login u] :: IO
      [String]
  print q
  case q of
    [f] -> when (not (null f)) $ removeFile f
    _   -> return ()
  return responseOK

