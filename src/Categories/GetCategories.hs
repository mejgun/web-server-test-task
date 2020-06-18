{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.GetCategories
  ( get
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Cat = Cat
    { id     :: Int
    , name   :: String
    , parent :: Maybe Int
    }
    deriving (Generic, Show)

instance FromRow Cat
instance A.ToJSON Cat

data Req = Req
    { page :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

get :: MyHandler Req [Cat]
get conn _ u =
  rIfValidPage (page u)
    $ handleSqlErr
    $ OkJSON
    <$> (query conn
               "select id,name,parent from categories offset ? limit ?;"
               (offset, limit) :: IO [Cat]
        )
 where
  offset = calcOffset (page u) categoriesPerPage
  limit  = categoriesPerPage
