{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.GetUsers
  ( getUsers
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           Text.Read                      ( readMaybe )

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

getUsers :: String -> MyApp
getUsers page conn _ respond =
  handle (checkSqlErr (respond responseSQLERR)) $ do
    users <-
      query conn
            "select name,lastname,photo from users offset ? limit ?;"
            [offset, limit] :: IO [User]
    respond $ responseJSON users
 where
  offset = maybe 0 (\p -> (p - 1) * usersPerPage) (readMaybe page :: Maybe Int)
  limit  = usersPerPage
