{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.DeleteNewsPhoto
  ( deleteNewsPhoto
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           System.Directory               ( removeFile )


import           PG
import           Types

data Req = Req
    { photo_id :: Int
    , news_id  :: Int
    , token    :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

deleteNewsPhoto :: MyHandler Req
deleteNewsPhoto conn u = handleSqlErr $ do
  p <-
    query
      conn
      "delete from news_photos where id=? and news_id=(select id from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?))) returning photo;"
      (photo_id u, news_id u, token u) :: IO [Maybe (Only String)]
  case p of
    [Just (Only f)] -> removeFile f >> return responseOK
    _               -> return responseSQLERR
