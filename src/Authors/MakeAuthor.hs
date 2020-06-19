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

make :: MyHandler Req String
make conn _ u =
  isAdmin conn (token u)
    >>  ifLoginExist conn (login u)
    >>  liftIO
          (execute
            conn
            "insert into authors (user_id,descr) values ((select id from users where login=?),?) on conflict (user_id) do update set descr=?;"
            [login u, descr u, descr u]
          )
    >>= execResult
