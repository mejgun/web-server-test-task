{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.EditAuthor
  ( edit
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { login :: String
    , token :: String
    , descr :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

edit :: MyHandler Req String
edit conn _ u =
  isAdmin conn (token u)
    >>  ifAuthorExist conn (login u)
    >>  execute
          conn
          "update authors set descr=? where user_id=(select id from users where login=?);"
          [descr u, login u]
    >>= execResult
