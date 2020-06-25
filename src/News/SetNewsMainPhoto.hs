{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.SetNewsMainPhoto
  ( setMainPhoto
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib
import qualified Lib.Logger                    as Logger

data Req = Req
    { news_id    :: Int
    , token      :: String
    , photo      :: String
    , photo_type :: Maybe String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

setMainPhoto :: MyHandler Req String
setMainPhoto conn logg u =
  isAuthor conn (token u)
    >> ifNewsExist conn (news_id u)
    >> ifNewsAuthor conn (news_id u) (token u)
    >> do
         p <-
           query
             conn
             "select main_photo from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
             (news_id u, token u) :: IO [Maybe (Only String)]
         case p of
           [Just (Only f)] ->
             logg Logger.LogDebug ("Removing file " ++ show (f))
               >> deleteFile logg f
           _ -> return ()
         let img = decodeBase64 $ photo u
             ext = makeExt $ photo_type u
         q <-
           query
             conn
             "update news set main_photo=concat(?,md5(random()::text),?) where id=? and author_id=(select id from authors where user_id=(select id from users where token=?)) returning main_photo;"
             (imagesDir, ext, news_id u, token u) :: IO [Maybe (Only String)]
         case q of
           [Just (Only imgFile)] -> saveFile logg imgFile img >> return ok
           _                     -> throw ErrorBadRequest
