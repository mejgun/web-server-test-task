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

data EditAuthor = EditAuthor
    { login :: String
    , token :: String
    , descr :: String
    }
    deriving (Generic, Show)

instance A.FromJSON EditAuthor

editAuthor :: MyHandler EditAuthor
editAuthor conn respond u =
  rIfAdmin conn respond (token u)
    $ handle (checkSqlErr (respond responseSQLERR))
    $ do
        _ <- execute
          conn
          "update authors set descr=? where user_id = (select id from users where login=?);"
          [descr u, login u]
        respond responseOK
