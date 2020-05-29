{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.EditTag
  ( editTag
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

editTag :: MyHandler Req
editTag conn u =
  rIfAdmin conn (token u)
    $  handleSqlErr
    $  execute conn "update tags set name=? where id=?;" (name u, tag_id u)
    >> return responseOK
