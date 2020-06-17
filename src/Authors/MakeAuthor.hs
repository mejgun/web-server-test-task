{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.MakeAuthor
  ( make
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { login :: String
    , token :: String
    , descr :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

make :: MyHandler Req Bool
make conn u =
  rIfAdmin conn (token u)
    $   rIfLoginExist conn (login u)
    $   handleSqlErr
    $   execute
          conn
          "insert into authors (user_id,descr) values ((select id from users where login=?),?) on conflict (user_id) do update set descr=?;"
          [login u, descr u, descr u]
    >>= rExecResult
