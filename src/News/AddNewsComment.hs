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
  i <-
    query conn "select id from news where id=? and published=true;" [news_id u] :: IO
      [Only Int]
  case i of
    [Only x] -> do
      q <- execute
        conn
        "insert into news_comments (news_id,text,user_id) values (?,?,(select id from users where token=?)) on conflict do nothing;"
        (x, text u, token u)
      case q of
        1 -> return responseOK
        _ -> return responseSQLERR
    _ -> return responseSQLERR
