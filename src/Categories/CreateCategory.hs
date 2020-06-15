{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.CreateCategory
  ( create
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { name   :: String
    , parent :: Maybe Int
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

create :: MyHandler Req Bool
create conn u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   execute
          conn
          "insert into categories (name,parent) values (?,(select id from categories where id=?)) on conflict do nothing;"
          (name u, parent u)
    >>= rExecResult
