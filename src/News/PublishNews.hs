{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.PublishNews
  ( release
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { news_id :: Int
    , token   :: String
    , publish :: Bool
    }
    deriving (Generic, Show)

instance A.FromJSON Req

release :: MyHandler Req String
release conn _ u =
  rIfAuthor conn (token u)
    >>  rIfNewsExist conn (news_id u)
    >>  rIfNewsAuthor conn (news_id u) (token u)
    >>  liftIO
          (execute
            conn
            "update news set published=? where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
            (publish u, news_id u, token u)
          )
    >>= rExecResult
