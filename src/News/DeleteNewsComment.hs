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

deleteComment :: MyHandler Req String
deleteComment conn _ u =
  isAdmin conn (token u)
    >>  liftIO
          (execute conn "delete from news_comments where id=?;" [comment_id u])
    >>= execResult
