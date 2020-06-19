{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.SetNewsMainPhoto
  ( setMainPhoto
  )
where

import qualified Data.Aeson                    as A
import qualified Data.ByteString               as B
import           Data.ByteString.Base64         ( decodeLenient )
import           Data.ByteString.UTF8           ( fromString )
import           Data.Char                      ( toLower )
import           GHC.Generics
import           System.Directory               ( removeFile )

import           Lib

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
         p <- liftIO
           (query
             conn
             "select main_photo from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
             (news_id u, token u) :: IO [Maybe (Only String)]
           )
         case p of
           [Just (Only f)] -> liftIO
             (logg LogDebug ("Removing file " ++ show (f)) >> removeFile f)
           _ -> return ()
         let img = decodeLenient $ fromString $ photo u
             ext = maybe ".jpg" ((++) "." . (map toLower)) (photo_type u)
         q <- liftIO
           (query
             conn
             "update news set main_photo=concat(?,md5(random()::text),?) where id=? and author_id=(select id from authors where user_id=(select id from users where token=?)) returning main_photo;"
             (imagesDir, ext, news_id u, token u) :: IO [Maybe (Only String)]
           )
         case q of
           [Just (Only imgFile)] ->
             liftIO (B.writeFile imgFile img) >> return ok
           _ -> throwError ErrorBadRequest
