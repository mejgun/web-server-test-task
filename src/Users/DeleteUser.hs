{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.DeleteUser
  ( delete
  )
where

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

delete :: MyHandler Req
delete conn u = rIfAdmin conn (token u) $ handleSqlErr $ do
  q <-
    query conn "delete from users where login=? returning photo;" [login u] :: IO
      [Maybe (Only String)]
  case q of
    [Just (Only f)] -> removeFile f >> return responseOK
    [Nothing      ] -> return responseOK
    _               -> return responseSQLERR

