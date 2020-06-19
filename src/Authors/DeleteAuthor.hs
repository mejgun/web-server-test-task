{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.DeleteAuthor
  ( delete
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

delete :: MyHandler Req String
delete conn _ u =
  rIfAdmin conn (token u)
    >>  rIfAuthorExist conn (login u)
    >>  liftIO
          (execute
            conn
            "delete from authors where user_id=(select id from users where login=?);"
            [login u]
          )
    >>= rExecResult
