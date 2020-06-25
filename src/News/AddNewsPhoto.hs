{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.AddNewsPhoto
  ( addPhoto
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib
import           Lib.FSUtils                    ( saveFile )

data Req = Req
    { news_id    :: Int
    , token      :: String
    , photo      :: String
    , photo_type :: Maybe String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

addPhoto :: MyHandler Req String
addPhoto conn logg u =
  isAuthor conn (token u)
    >> ifNewsExist conn (news_id u)
    >> ifNewsAuthor conn (news_id u) (token u)
    >> do
         let img = decodeBase64 $ photo u
             ext = makeExt $ photo_type u
         q <- query
           conn
           "insert into news_photos (news_id,photo) values ((select id from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?))),concat(?,md5(random()::text),?)) returning photo;"
           (news_id u, token u, imagesDir, ext)
         case q of
           [Only imgFile] -> saveFile logg imgFile img >> return ok
           _              -> throw ErrorBadRequest

