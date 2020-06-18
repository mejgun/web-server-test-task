{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.DeleteCategory
  ( delete
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { cat_id :: Int
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

delete :: MyHandler Req Bool
delete conn _ u =
  rIfAdmin conn (token u)
    $   rIfCategoryExist conn (cat_id u)
    $   execute conn "delete from categories where id=?;" [cat_id u]
    >>= rExecResult
