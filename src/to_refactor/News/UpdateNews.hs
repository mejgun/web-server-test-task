{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module News.UpdateNews
  ( update
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req =
  Req
    { news_id :: Int
    , name :: String
    , cat_id :: Int
    , text :: String
    , token :: String
    }
  deriving (Generic, Show)

instance A.FromJSON Req

update :: MyHandler Req String
update conn _ u =
  isAuthor conn (token u)
    >>  ifNewsExist conn (news_id u)
    >>  ifNewsAuthor conn (news_id u) (token u)
    >>  ifCategoryExist conn (cat_id u)
    >>  execute
          conn
          "update news set name=?, category_id=?, text=? where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
          (name u, cat_id u, text u, news_id u, token u)
    >>= execResult
