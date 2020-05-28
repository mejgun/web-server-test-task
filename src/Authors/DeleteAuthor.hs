{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.DeleteAuthor
  ( deleteAuthor
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data DeleteAuthor = DeleteAuthor
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON DeleteAuthor

deleteAuthor :: MyHandler DeleteAuthor
deleteAuthor conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute
      conn
      "delete from authors where user_id=(select id from users where login=?);"
      [login u]
    respond responseOK


