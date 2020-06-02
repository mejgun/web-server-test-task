{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.AddNewsComment
  ( addComment
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { news_id :: Int
    , token   :: String
    , text    :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

addComment :: MyHandler Req
addComment conn u = handleSqlErr $ do
  q <- execute
    conn
    "insert into news_comments (news_id,text,user_id) values (?,?,(select id from users where token=?)) on conflict do nothing;"
    (news_id u, text u, token u)
  return $ case q of
    1 -> responseOK
    _ -> responseSQLERR
