{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.CreateUser
  ( createUser
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           Data.ByteString.Base64         ( decodeLenient )
import           Data.ByteString.UTF8           ( fromString )
import           Data.Char                      ( toLower )
import qualified Data.ByteString               as B

import           PG
import           Types

data Req = Req
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    , photo_type :: Maybe String
    , login    :: String
    , password :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

createUser :: MyHandler Req
createUser conn respond u = case photo u of
  Nothing -> handleSqlErr respond $ do
    _ <- execute
      conn
      "insert into users (name,lastname,token,login,password) values(?,?,md5(random()::text),?,md5(?)) on conflict do nothing;"
      (name u, lastname u, login u, password u)
    respond responseOK
  Just ph -> do
    let img = decodeLenient $ fromString ph
        ext = maybe ".jpg" ((++) "." . (map toLower)) (photo_type u)
    q <- query
      conn
      "insert into users (name,lastname,token,login,password,photo) values(?,?,md5(random()::text),?,md5(?),concat(?,md5(random()::text),?)) on conflict do nothing returning photo;"
      (name u, lastname u, login u, password u, imagesDir, ext)
    case q of
      [Only imgFile] -> do
        B.writeFile imgFile img
        respond responseOK
      _ -> respond responseERR
