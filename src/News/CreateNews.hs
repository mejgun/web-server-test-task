{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.CreateNews
  ( createNews
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data NewsId = NewsId
    { news_id :: Int
    }
    deriving (Generic, Show)

instance FromRow NewsId
instance A.ToJSON NewsId

data Req = Req
    { name   :: String
    , cat_id :: Int
    , text   :: String
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

createNews :: MyHandler Req
createNews conn u = rIfAuthor conn (token u) $ handleSqlErr $ do
  q <-
    query
      conn
      "insert into news (name,date,author_id,category_id,text) values (?,now(),(select id from authors where user_id=(select id from users where token=?)),?,?) returning id;"
      (name u, token u, cat_id u, text u) :: IO [NewsId]
  return $ case q of
    [n] -> respJSON n
    _   -> responseSQLERR


