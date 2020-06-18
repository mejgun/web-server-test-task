{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.DeleteNewsComment
  ( deleteComment
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { comment_id :: Int
    , token      :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

deleteComment :: MyHandler Req Bool
deleteComment conn _ u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   execute conn "delete from news_comments where id=?;" [comment_id u]
    >>= rExecResult
