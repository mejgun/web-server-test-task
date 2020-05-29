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

data Req = Req
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

deleteAuthor :: MyHandler Req
deleteAuthor conn u =
  rIfAdmin conn (token u)
    $  handleSqlErr
    $  execute
         conn
         "delete from authors where user_id=(select id from users where login=?);"
         [login u]
    >> return responseOK


