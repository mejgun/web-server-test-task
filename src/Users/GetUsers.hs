{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.GetUsers
  ( get
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data User = User
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    }
    deriving (Generic, Show)

instance FromRow User
instance A.ToJSON User

data Req = Req
    { page :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

get :: MyHandler Req [User]
get conn u =
  handleSqlErr
    $ OkJSON
    <$> (query conn
               "select name,lastname,photo from users offset ? limit ?;"
               [offset, limit] :: IO [User]
        )
 where
  offset = calcOffset (page u) usersPerPage
  limit  = usersPerPage
