{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Authors.MakeAuthor
  ( makeAuthor
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           PG
import           Types

data MakeAuthor = MakeAuthor
    { login :: String
    , token :: String
    , descr :: String
    }
    deriving (Generic, Show)

instance A.FromJSON MakeAuthor

makeAuthor :: MyApp
makeAuthor conn req respond = do
  p <- bodyToJSON req :: IO (Maybe MakeAuthor)
  print p
  maybe
    (respond responseERR)
    (\u -> do
      adm <- isAdmin conn $ token u
      if adm
        then handle (checkSqlErr (respond responseSQLERR)) $ do
          _ <- execute
            conn
            "insert into authors (user_id,descr) values ((select id from users where login=?),?);"
            [login u, descr u]
          respond responseOK
        else respond responseERR
    )
    p
