{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.CreateTag
  ( create
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { name  :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

create :: MyHandler Req String
create conn _ u =
  isAdmin conn (token u)
    >>  ifTagNotExist conn (name u)
    >>  execute conn
                "insert into tags (name) values(?) on conflict do nothing;"
                [name u]
    >>= execResult
