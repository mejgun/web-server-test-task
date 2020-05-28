{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.CreateCategory where

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
createCategory conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute
      conn
      "insert into categories (name,parent) values(?,?) on conflict do nothing;"
      (name u, parent u)
    respond responseOK
