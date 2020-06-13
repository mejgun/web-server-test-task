{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.MakeAuthor
  ( make
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

make :: MyHandler Req
make conn u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   execute
          conn
          "insert into authors (user_id,descr) values ((select id from users where login=?),?) on conflict do nothing;"
          [login u, descr u]
    >>= rExecResult
