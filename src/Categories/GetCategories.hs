{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.GetCategories
  ( getCategories
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

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

getCategories :: MyHandler Req
getCategories conn u =
  handleSqlErr
    $ respJSON
    <$> (query conn
               "select id,name,parent from categories offset ? limit ?;"
               (offset (page u), limit) :: IO [Cat]
        )
 where
  offset i = (i - 1) * categoriesPerPage
  limit = categoriesPerPage
