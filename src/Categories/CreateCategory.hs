{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.CreateCategory
  ( create
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { name   :: String
    , parent :: Maybe Int
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

create :: MyHandler Req String
create conn _ u =
  isAdmin conn (token u)
    >>  liftIO
          (execute
            conn
            "insert into categories (name,parent) values (?,(select id from categories where id=?)) on conflict do nothing;"
            (name u, parent u)
          )
    >>= execResult
