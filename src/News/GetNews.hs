{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.GetNews
  ( get
  )
where


import qualified Data.Aeson                    as A
import           Data.List                      ( sort )
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple.Time
                                                ( Date )
import           GHC.Generics
import           PG
import           Types

data News = News
    { n_id              :: Int
    , n_date            :: Date
    , n_name            :: String
    , n_text            :: String
    , n_main_photo      :: Maybe String
    , n_author_name     :: String
    , n_author_lastname :: String
    , n_category_id     :: Int
    , n_category_name   :: String
    , n_photo_count     :: Int
    }
    deriving (Generic, Show)

instance FromRow News

data Req = Req
    { created_at        :: Maybe String
    , created_before    :: Maybe String
    , created_after     :: Maybe String
    , author_contains   :: Maybe String
    , name_contains     :: Maybe String
    , text_contains     :: Maybe String
    , anything_contains :: Maybe String
    , cat_id            :: Maybe Int
    , tags_all          :: Maybe [Int]
    , tags_any          :: Maybe [Int]
    , sort_by           :: Maybe String
    , page              :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

get :: MyHandler Req
get conn u = handleSqlErr $ do
  news <-
    query
      conn
      "select * from getNews(?,?,?,?,?,?,?,?,?,?,?,?,?);"
      ( created_at u
      , created_before u
      , created_after u
      , author_contains u
      , name_contains u
      , text_contains u
      , anything_contains u
      , cat_id u
      , t_all
      , t_any
      , sort_by u
      , offset
      , limit
      ) :: IO [News]
  print news
  print $ map n_id news
  tags <-
    query
      conn
      "select nt.news_id,nt.tag_id,t.name from news_tags as nt left join tags as t on t.id=nt.tag_id where nt.news_id in ?;"
    $ Only
    $ In
    $ map n_id news :: IO [(Int, Int, String)]
  print tags
  return responseOK
 where
  offset = ((page u) - 1) * newsPerPage
  limit  = newsPerPage
  t_all  = intListToPGarray <$> tags_all u
  t_any  = intListToPGarray <$> tags_any u

intListToPGarray :: [Int] -> String
intListToPGarray l = T.unpack t2
 where
  t1 = T.intercalate "," $ map (T.pack . show) $ sort l
  t2 = T.concat ["{", t1, "}"]
