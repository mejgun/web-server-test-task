{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.PublishNews
  ( publishNews
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { news_id :: Int
    , token   :: String
    , publish :: Bool
    }
    deriving (Generic, Show)

instance A.FromJSON Req

publishNews :: MyHandler Req
publishNews conn u = handleSqlErr $ do
  q <- execute
    conn
    "update news set published=? where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
    (publish u, news_id u, token u)
  return $ case q of
    1 -> responseOK
    _ -> responseSQLERR



