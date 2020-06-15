{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.DeleteTag
  ( delete
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { tag_id :: Int
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

delete :: MyHandler Req Bool
delete conn u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   execute conn "delete from tags where id=?;" [tag_id u]
    >>= rExecResult
