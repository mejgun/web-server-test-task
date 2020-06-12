{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.AddComment
  ( add
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics
import           PG
import           Types

data Req = Req
    { token   :: String
    , news_id :: Int
    , text    :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

add :: MyHandler Req
add conn u = handleSqlErr $ do
  r <- execute
    conn
    "insert into news_comments (news_id,text,user_id) values ((select id from news where id=? and published=true),?,(select id from users where token=?)) on conflict do nothing;"
    (news_id u, text u, token u)
  return $ case r of
    1 -> responseOK
    _ -> responseSQLERR

