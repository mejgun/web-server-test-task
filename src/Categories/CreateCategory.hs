{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.CreateCategory
  ( createCategory
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { name   :: String
    , parent :: Maybe Int
    , token  :: String
    }
    deriving (Generic, Show)


instance A.FromJSON Req

createCategory :: MyHandler Req
createCategory conn u =
  rIfAdmin conn (token u)
    $  handleSqlErr
    $  execute
         conn
         "insert into categories (name,parent) values (?,(select id from categories where id=?)) on conflict do nothing;"
         (name u, parent u)
    >> return responseOK
