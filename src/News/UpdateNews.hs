{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.UpdateNews
  ( updateNews
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { news_id :: Int
    , name    :: String
    , cat_id  :: Int
    , text    :: String
    , token   :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

updateNews :: MyHandler Req
updateNews conn u = handleSqlErr $ do
  q <- execute
    conn
    "update news set name=?, category_id=?, text=? where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
    (name u, cat_id u, text u, news_id u, token u)
  return $ case q of
    1 -> responseOK
    _ -> responseSQLERR



