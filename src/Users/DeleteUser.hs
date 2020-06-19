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

delete :: MyHandler Req String
delete conn logg u =
  isAdmin conn (token u) >> ifLoginExist conn (login u) >> do
    q <-
      query conn "delete from users where login=? returning photo;" [login u] :: IO
        [Maybe (Only String)]
    case q of
      [Just (Only f)] ->
        logg LogDebug ("Removing file " ++ show (f))
          >> removeFile f
          >> return ok
      [Nothing] -> return ok
      _         -> throw ErrorBadRequest
