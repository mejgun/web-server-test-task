{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.AddNewsTag
  ( addTag
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { news_id :: Int
    , tag_id  :: Int
    , token   :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

addTag :: MyHandler Req String
addTag conn _ u =
  isAuthor conn (token u)
    >>  ifNewsExist conn (news_id u)
    >>  ifNewsAuthor conn (news_id u) (token u)
    >>  ifTagExist conn (tag_id u)
    >>  liftIO
          (execute
            conn
            "insert into news_tags (tag_id,news_id) values (?,(select id from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?)))) on conflict (tag_id,news_id) do update set tag_id=?;"
            (tag_id u, news_id u, token u, tag_id u)
          )
    >>= execResult
