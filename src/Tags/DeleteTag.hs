{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.DeleteTag
  ( deleteTag
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data Req = Req
    { tag_id :: Int
    , token  :: String
    }
    deriving (Generic, Show)


instance A.FromJSON Req

deleteTag :: MyHandler Req
deleteTag conn respond u =
  rIfAdmin conn respond (token u) $ handleSqlErr respond $ do
    _ <- execute conn "delete from tags where id=?;" [tag_id u]
    respond responseOK
