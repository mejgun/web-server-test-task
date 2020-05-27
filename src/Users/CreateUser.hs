{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.CreateUser
  ( createUser
  )
where

import qualified Data.Aeson                    as A
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics

import           PG
import           Types

data CreateUser = CreateUser
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    , login    :: String
    , password :: String
    }
    deriving (Generic, Show)

instance A.FromJSON CreateUser

createUser :: MyApp
createUser conn req respond = do
  p <- bodyToJSON req :: IO (Maybe CreateUser)
  maybe
    (respond responseERR)
    (\u -> do
      let img = fromMaybe "" (photo u)
      handle (checkSqlErr (respond responseSQLERR)) $ do
        _ <- execute
          conn
          "insert into users (name,lastname,photo,token,login,password) values(?,?,?,md5(random()::text),?,md5(?)) on conflict do nothing;"
          [name u, lastname u, img, login u, password u]
        respond responseOK
    )
    p
