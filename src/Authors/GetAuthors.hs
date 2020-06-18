{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.GetAuthors
  ( get
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

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

get :: MyHandler Req [Author]
get conn _ u =
  rIfAdmin conn (token u)
    $   rIfValidPage (page u)
    $   handleSqlErr
    $   OkJSON
    <$> (query
          conn
          "select name,lastname,photo,descr from authors as a,users as u where a.user_id=u.id offset ? limit ?;"
          [offset, limit] :: IO [Author]
        )
 where
  offset = calcOffset (page u) usersPerPage
  limit  = authorsPerPage
