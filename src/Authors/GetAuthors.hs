{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.GetAuthors
  ( get
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Author = Author
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    , descr    :: String
    }
    deriving (Generic, Show)

instance FromRow Author
instance A.ToJSON Author

data Req = Req
    { token :: String
    , page  :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

get :: MyHandler Req
get conn u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   respJSON
    <$> (query
          conn
          "select name,lastname,photo,descr from authors as a,users as u where a.user_id=u.id offset ? limit ?;"
          [offset (page u), limit] :: IO [Author]
        )
 where
  offset i = (i - 1) * usersPerPage
  limit = authorsPerPage
