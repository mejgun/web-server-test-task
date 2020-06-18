{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.DeleteUser
  ( delete
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           System.Directory               ( removeFile )

import           Lib

data Req = Req
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

delete :: MyHandler Req Bool
delete conn _ u = rIfAdmin conn (token u) $ rIfLoginExist conn (login u) $ do
  q <-
    query conn "delete from users where login=? returning photo;" [login u] :: IO
      [Maybe (Only String)]
  case q of
    [Just (Only f)] -> removeFile f >> return Ok200
    [Nothing      ] -> return Ok200
    _               -> return ErrorBadRequest
