{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.DeleteNewsTag
  ( deleteTag
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { news_id :: Int
    , tag_id  :: Int
    , token   :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

deleteTag :: MyHandler Req
deleteTag conn u =
  rIfAuthor conn (token u)
    $   handleSqlErr
    $   execute
          conn
          "delete from news_tags where tag_id=? and news_id=(select id from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?)));"
          (tag_id u, news_id u, token u)
    >>= rExecResult


