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

data Req = Req
    { page :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

getUsers :: MyApp
getUsers conn req respond = do
  p <- bodyToJSON req :: IO (Maybe Req)
  maybe
    (respond responseERR)
    (\u -> do
      handle (checkSqlErr (respond responseSQLERR)) $ do
        users <-
          query conn
                "select name,lastname,photo from users offset ? limit ?;"
                [offset (page u), limit] :: IO [User]
        respond $ responseJSON users
    )
    p
 where
  offset i = (i - 1) * usersPerPage
  limit = usersPerPage
