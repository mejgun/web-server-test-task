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

data TempNews = TempNews
    { n_id              :: Int
    , n_date            :: Date
    , n_name            :: String
    , n_text            :: String
    , n_main_photo      :: Maybe String
    , n_author_name     :: String
    , n_author_lastname :: String
    , n_category_id     :: Int
    , n_photo_count     :: Int
    }
    deriving (Generic, Show)

instance FromRow TempNews

data TempCat = TempCat
    { c_id     :: Int
    , c_name   :: String
    , c_parent :: Maybe Int
    }
    deriving (Generic, Show)

instance FromRow TempCat

data TempTag = TempTag
    { t_n_id :: Int
    , t_id   :: Int
    , t_name :: String
    }
    deriving (Generic, Show)

instance FromRow TempTag

data Category = Category
    { category_id     :: Int
    , category_name   :: String
    , category_parent :: Maybe Category
    }
    deriving (Generic, Show)

instance A.FromJSON Category

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
      ) :: IO [TempNews]
  -- print news
  tags <-
    query
      conn
      "select nt.news_id,nt.tag_id,t.name from news_tags as nt left join tags as t on t.id=nt.tag_id where nt.news_id in ?;"
    $ Only
    $ In
    $ map n_id news :: IO [TempTag]
  print tags
  cats <- query_ conn "select id,name,parent from categories;" :: IO [TempCat]
  -- print cats
  print $ map (\t -> buildCategories (n_category_id t) cats) news
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

buildCategories :: Int -> [TempCat] -> Category
buildCategories c cats =
  let tempc = head $ filter (\i -> c_id i == c) cats
  in  Category
        { category_id     = c_id tempc
        , category_name   = c_name tempc
        , category_parent = case c_parent tempc of
                              Nothing -> Nothing
                              Just i  -> Just $ buildCategories i cats
        }
