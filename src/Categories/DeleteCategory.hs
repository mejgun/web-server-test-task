{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.DeleteCategory
  ( delete
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

delete :: MyHandler Req Bool
delete conn u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   execute conn "delete from categories where id=?;" [cat_id u]
    >>= rExecResult
