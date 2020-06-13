{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.EditTag
  ( edit
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { tag_id :: Int
    , name   :: String
    , token  :: String
    }
    deriving (Generic, Show)


instance A.FromJSON Req

edit :: MyHandler Req
edit conn u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   execute conn "update tags set name=? where id=?;" (name u, tag_id u)
    >>= rExecResult
