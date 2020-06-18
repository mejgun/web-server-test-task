{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.GetNewsComments
  ( getComments
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Comment = Comment
    { comment_id    :: Int
    , user_name     :: String
    , user_lastname :: String
    , comment       :: String
    }
    deriving (Generic, Show)

instance FromRow Comment
instance A.ToJSON Comment

data Req = Req
    { news_id :: Int
    , page    :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

getComments :: MyHandler Req [Comment]
getComments conn _ u =
  rIfValidPage (page u)
    $   rIfNewsPublished conn (news_id u)
    $   handleSqlErr
    $   OkJSON
    <$> (query
          conn
          "select c.id,u.name,u.lastname,c.text from news_comments as c, users as u, news as n where c.user_id=u.id and news_id=? and n.id=c.news_id and n.published=true offset ? limit ?;"
          (news_id u, offset, limit) :: IO [Comment]
        )
 where
  offset = calcOffset (page u) commentsPerPage
  limit  = commentsPerPage
