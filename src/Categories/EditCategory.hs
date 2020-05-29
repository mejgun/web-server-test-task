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
editCategory conn u =
  rIfAdmin conn (token u)
    $  handleSqlErr
    $  execute conn
               "update categories set name=?, parent=? where id=?;"
               (name u, parent u, cat_id u)
    >> return responseOK
