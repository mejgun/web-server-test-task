{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.EditAuthor
  ( editAuthor
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data EditAuthor = EditAuthor
    { login :: String
    , token :: String
    , descr :: String
    }
    deriving (Generic, Show)

instance A.FromJSON EditAuthor

editAuthor :: MyApp
editAuthor conn req respond = do
  p <- bodyToJSON req :: IO (Maybe EditAuthor)
  maybe
    (respond responseERR)
    (\u -> do
      adm <- isAdmin conn $ token u
      if adm
        then handle (checkSqlErr (respond responseSQLERR)) $ do
          _ <- execute
            conn
            "update authors set descr=? where user_id = (select id from users where login=?);"
            [descr u, login u]
          respond responseOK
        else respond responseERR
    )
    p
