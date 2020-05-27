{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.DeleteAuthor
  ( deleteAuthor
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data DeleteAuthor = DeleteAuthor
    { login :: String
    , token :: String
    }
    deriving (Generic, Show)

instance A.FromJSON DeleteAuthor

deleteAuthor :: MyApp
deleteAuthor conn req respond = do
  p <- bodyToJSON req :: IO (Maybe DeleteAuthor)
  maybe
    (respond responseERR)
    (\u -> do
      adm <- isAdmin conn $ token u
      if adm
        then handle (checkSqlErr (respond responseSQLERR)) $ do
          _ <- execute
            conn
            "delete from authors where user_id=(select id from users where login=?);"
            [login u]
          respond responseOK
        else respond responseERR
    )
    p
