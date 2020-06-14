{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.CreateTag
  ( create
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { name  :: String
    , token :: String
    }
    deriving (Generic, Show)


instance A.FromJSON Req

create :: MyHandler Req Bool
create conn u =
  rIfAdmin conn (token u)
    $   handleSqlErr
    $   execute conn
                "insert into tags (name) values(?) on conflict do nothing;"
                [name u]
    >>= rExecResult
