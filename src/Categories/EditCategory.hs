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

edit :: MyHandler Req String
edit conn _ u =
  isAdmin conn (token u)
    >>  ifCategoryExist conn (cat_id u)
    >>  execute conn
                "update categories set name=?, parent=? where id=?;"
                (name u, parent u, cat_id u)
    >>= execResult
