{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tags.EditTag
  ( edit
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { tag_id :: Int
    , name   :: String
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

edit :: MyHandler Req String
edit conn _ u =
  rIfAdmin conn (token u)
    >>  rIfTagExist conn (tag_id u)
    >>  liftIO
          (execute conn "update tags set name=? where id=?;" (name u, tag_id u))
    >>= rExecResult
