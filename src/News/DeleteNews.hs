{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.DeleteNews
  ( delete
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           System.Directory               ( removeFile )

import           Lib

data Req = Req
    { news_id :: Int
    , token   :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

delete :: MyHandler Req String
delete conn logg u =
  isAuthor conn (token u)
    >> ifNewsExist conn (news_id u)
    >> ifNewsAuthor conn (news_id u) (token u)
    >> do
         pf <-
           query
             conn
             "delete from news_photos where news_id=(select id from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?))) returning photo;"
             (news_id u, token u) :: IO [Only String]
         mapM_ (\f -> removeFile (fromOnly f)) pf
         mf <-
           query
             conn
             "delete from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?)) returning main_photo;"
             (news_id u, token u) :: IO [Maybe (Only String)]
         case mf of
           [Just (Only f)] ->
             logg LogDebug ("Removing file " ++ show (f))
               >> removeFile f
               >> return ok
           [Nothing] -> return ok
           _         -> throw ErrorBadRequest
