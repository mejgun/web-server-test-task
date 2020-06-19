{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.GetTags
  ( get
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Tag = Tag
    { id   :: Int
    , name :: String
    }
    deriving (Generic, Show)

instance FromRow Tag
instance A.ToJSON Tag

data Req = Req
    { page :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

get :: MyHandler Req [Tag]
get conn _ u =
  rIfValidPage (page u)
    >>  liftIO
          (query conn
                 "select id,name from tags offset ? limit ?;"
                 (offset, limit) :: IO [Tag]
          )
    >>= return
 where
  offset = calcOffset (page u) tagsPerPage
  limit  = tagsPerPage
