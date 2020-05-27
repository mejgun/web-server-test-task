{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.GetAuthors
  ( getAuthors
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

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

data Req = Req
    { token :: String
    , page  :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

getAuthors :: MyApp
getAuthors conn req respond = do
  p <- bodyToJSON req :: IO (Maybe Req)
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
              [offset (page u), limit] :: IO [Author]
          respond $ responseJSON users
        else respond responseERR
    )
    p
 where
  offset i = (i - 1) * usersPerPage
  limit = authorsPerPage
