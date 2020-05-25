{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.CreateUser
  ( createUser
  )
where

import qualified Data.Aeson                    as A
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics
import           Network.HTTP.Types             ( status200
                                                , status404
                                                )
import           Network.Wai


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

instance A.ToJSON CreateUser
instance A.FromJSON CreateUser

createUser :: MyApp
createUser conn req respond = do
  b <- lazyRequestBody req
  let p = A.decode b :: Maybe CreateUser
  print p
  case p of
    Just u -> do
      let img = fromMaybe "" (photo u)
      _ <- execute
        conn
        "insert into users (name,lastname,photo,token,login,password) values(?,?,?,md5(random()::text),?,md5(?));"
        [name u, lastname u, img, login u, password u]
      respond $ responseBuilder status200 [] ok
    _ -> respond $ responseBuilder status404 [] err
