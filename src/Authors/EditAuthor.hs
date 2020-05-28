{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.EditAuthor
  ( editAuthor
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { login :: String
    , token :: String
    , descr :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

editAuthor :: MyHandler Req
editAuthor conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute
      conn
      "update authors set descr=? where user_id = (select id from users where login=?);"
      [descr u, login u]
    respond responseOK
