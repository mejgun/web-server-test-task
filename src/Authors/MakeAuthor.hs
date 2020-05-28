{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.MakeAuthor
  ( makeAuthor
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

makeAuthor :: MyHandler Req
makeAuthor conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute
      conn
      "insert into authors (user_id,descr) values ((select id from users where login=?),?) on conflict do nothing;"
      [login u, descr u]
    respond responseOK

