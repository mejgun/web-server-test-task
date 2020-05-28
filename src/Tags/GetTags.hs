{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.GetTags
  ( getTags
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Tag = Tag
    { id     :: Int
    , name   :: String
    }
    deriving (Generic, Show)

instance FromRow Tag
instance A.ToJSON Tag

data Req = Req
    { page :: Int
    }
    deriving (Generic, Show)


instance A.FromJSON Req

getTags :: MyHandler Req
getTags conn respond u = handleSqlErr respond $ do
  tags <-
    query conn
          "select id,name from tags offset ? limit ?;"
          (offset (page u), limit) :: IO [Tag]
  respond $ responseJSON tags
 where
  offset i = (i - 1) * tagsPerPage
  limit = tagsPerPage
