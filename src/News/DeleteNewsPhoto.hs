{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.DeleteNewsPhoto
  ( deletePhoto
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib
import           Lib.FSUtils                    ( deleteFile )
import qualified Lib.Logger                    as Logger

data Req = Req
    { photo_id :: Int
    , news_id  :: Int
    , token    :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

deletePhoto :: MyHandler Req String
deletePhoto conn logg u =
  isAuthor conn (token u)
    >> ifNewsExist conn (news_id u)
    >> ifNewsAuthor conn (news_id u) (token u)
    >> do
         p <-
           query
             conn
             "delete from news_photos where id=? and news_id=(select id from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?))) returning photo;"
             (photo_id u, news_id u, token u) :: IO [Maybe (Only String)]
         case p of
           [Just (Only f)] ->
             logg Logger.LogDebug ("Removing file " ++ show (f))
               >> deleteFile logg f
               >> return ok
           _ -> throw ErrorBadRequest
