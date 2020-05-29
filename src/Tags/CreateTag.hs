{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.CreateTag
  ( createTag
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

createTag :: MyHandler Req
createTag conn u =
  rIfAdmin conn (token u)
    $  handleSqlErr
    $  execute conn
               "insert into tags (name) values(?) on conflict do nothing;"
               [name u]
    >> return responseOK
