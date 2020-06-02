{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.AddNewsPhoto
  ( addPhoto
  )
where

import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import           Data.ByteString.Base64         ( decodeLenient )
import           Data.ByteString.UTF8           ( fromString )
import           Data.Char                      ( toLower )
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { news_id    :: Int
    , token      :: String
    , photo      :: String
    , photo_type :: Maybe String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

addPhoto :: MyHandler Req
addPhoto conn u = handleSqlErr $ do
  let img = decodeLenient $ fromString $ photo u
      ext = maybe ".jpg" ((++) "." . (map toLower)) (photo_type u)
  q <- query
    conn
    "insert into news_photos (news_id,photo) values ((select id from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?))),concat(?,md5(random()::text),?)) returning photo;"
    (news_id u, token u, imagesDir, ext)
  case q of
    [Only imgFile] -> B.writeFile imgFile img >> return responseOK
    _              -> return responseERR