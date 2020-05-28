{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.EditCategory
  ( editCategory
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { cat_id :: Int
    , name   :: String
    , parent :: Maybe Int
    , token  :: String
    }
    deriving (Generic, Show)


instance A.FromJSON Req

editCategory :: MyHandler Req
editCategory conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute conn
                 "update categories set name=?, parent=? where id=?;"
                 (name u, parent u, cat_id u)
    respond responseOK
