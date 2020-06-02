{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.DeleteNewsComment
  ( deleteComment
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { comment_id :: Int
    , token      :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

deleteComment :: MyHandler Req
deleteComment conn u = rIfAdmin conn (token u) $ handleSqlErr $ do
  q <- execute conn "delete from news_comments where id=?;" [comment_id u]
  return $ case q of
    1 -> responseOK
    _ -> responseSQLERR
