{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.EditCategory
  ( edit
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { cat_id :: Int
    , name   :: String
    , parent :: Maybe Int
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

edit :: MyHandler Req Bool
edit conn u =
  rIfAdmin conn (token u)
    $   rIfCategoryExist conn (cat_id u)
    $   handleSqlErr
    $   execute conn
                "update categories set name=?, parent=? where id=?;"
                (name u, parent u, cat_id u)
    >>= rExecResult
