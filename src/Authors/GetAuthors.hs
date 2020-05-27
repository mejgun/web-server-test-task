{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.GetAuthors
  ( getAuthors
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           Text.Read                      ( readMaybe )

import           PG
import           Types

data Author = Author
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    , descr    :: String
    }
    deriving (Generic, Show)

instance FromRow Author
instance A.ToJSON Author

data Token = Token
    { token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Token

getAuthors :: String -> MyApp
getAuthors page conn req respond = do
  p <- bodyToJSON req :: IO (Maybe Token)
  maybe
    (respond responseERR)
    (\u -> do
      adm <- isAdmin conn $ token u
      if adm
        then handle (checkSqlErr (respond responseSQLERR)) $ do
          users <-
            query
              conn
              "select name,lastname,photo,descr from authors as a,users as u where a.user_id=u.id offset ? limit ?;"
              [offset, limit] :: IO [Author]
          respond $ responseJSON users
        else respond responseERR
    )
    p
 where
  offset = maybe 0 (\p -> (p - 1) * usersPerPage) (readMaybe page :: Maybe Int)
  limit  = authorsPerPage
