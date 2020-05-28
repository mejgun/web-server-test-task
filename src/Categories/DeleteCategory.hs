{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.DeleteCategory
  ( deleteCategory
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { cat_id :: Int
    , token  :: String
    }
    deriving (Generic, Show)


instance A.FromJSON Req

deleteCategory :: MyHandler Req
deleteCategory conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute conn "delete from categories where id=?;" [cat_id u]
    respond responseOK
