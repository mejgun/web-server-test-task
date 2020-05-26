{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.LoginUser
  ( loginUser
  )
where

import           Blaze.ByteString.Builder       ( fromLazyByteString )
import qualified Data.Aeson                    as A
-- import           Data.Maybe                     ( catMaybes )
import           GHC.Generics
import           Network.HTTP.Types             ( status200 )
import           Network.Wai

import           PG
import           Types

data LoginUser = LoginUser
    { login    :: String
    , password :: String
    }
    deriving (Generic, Show)

instance A.ToJSON LoginUser
instance A.FromJSON LoginUser

data Token = Token
    { token :: String
    }
    deriving (Generic, Show)

instance FromRow Token
instance A.ToJSON Token
instance A.FromJSON Token

loginUser :: MyApp
loginUser conn req respond = do
  b <- lazyRequestBody req
  let p = A.decode b :: Maybe LoginUser
  case p of
    Just u -> handle (checkSqlErr (respond responseSQLERR)) $ do
      t <-
        query conn
              "select token from users where login = ? and password = md5(?);"
              [login u, password u] :: IO [Token]
      if null t
        then respond responseERR
        else
          respond
          $ responseBuilder status200 jsonCT
          $ fromLazyByteString
          $ A.encode
          $ head t
    _ -> respond responseERR
