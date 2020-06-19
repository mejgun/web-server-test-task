{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.CreateUser
  ( create
  )
where

import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import           Data.ByteString.Base64         ( decodeLenient )
import           Data.ByteString.UTF8           ( fromString )
import           Data.Char                      ( toLower )
import           GHC.Generics

import           Lib

data Req = Req
    { name       :: String
    , lastname   :: String
    , photo      :: Maybe String
    , photo_type :: Maybe String
    , login      :: String
    , password   :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

create :: MyHandler Req String
create conn _ u = ifLoginNotExist conn (login u) >> case photo u of
  Nothing ->
    liftIO
        (execute
          conn
          "insert into users (name,lastname,token,login,password) values(?,?,md5(random()::text),?,md5(?)) on conflict do nothing;"
          (name u, lastname u, login u, password u)
        )
      >>= execResult
  Just ph -> do
    let img = decodeLenient $ fromString ph
        ext = maybe ".jpg" ((++) "." . (map toLower)) (photo_type u)
    q <- liftIO
      (query
        conn
        "insert into users (name,lastname,token,login,password,photo) values(?,?,md5(random()::text),?,md5(?),concat(?,md5(random()::text),?)) on conflict do nothing returning photo;"
        (name u, lastname u, login u, password u, imagesDir, ext)
      )
    case q of
      [Only imgFile] -> liftIO (B.writeFile imgFile img) >> return ok
      _              -> throwError ErrorBadRequest
