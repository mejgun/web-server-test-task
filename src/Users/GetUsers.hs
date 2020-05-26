{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.GetUsers
  ( getUsers
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

getUsers :: MyApp
getUsers conn _ respond = handle (checkSqlErr (respond responseSQLERR)) $ do
  users <- query_ conn "select name,lastname,photo from users;" :: IO [User]
  respond $ responseJSON users
