{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.GetNews
  ( get
  )
where


import           Control.Monad                  ( liftM )
import qualified Data.Aeson                    as A
import           Data.List                      ( sort )
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple.FromRow
                                                ( field
                                                , fromRow
                                                )
import           GHC.Generics
import           PG
import           Types

data TempCat = TempCat
    { c_id     :: Int
    , c_name   :: String
    , c_parent :: Maybe Int
    }
    deriving (Generic, Show)

instance FromRow TempCat

data Category = Category
    { category_id     :: Int
    , category_name   :: String
    , category_parent :: Maybe Category
    }
    deriving (Generic, Show)

instance A.ToJSON Category

data News = News
    { news_id              :: Int
    , news_date            :: String
    , news_name            :: String
    , news_text            :: String
    , news_main_photo      :: Maybe String
    , news_author_name     :: String
    , news_author_lastname :: String
    , news_category_id     :: Int
    , news_category        :: Category
    , news_photos          :: [String]
    , news_tags            :: [String]
    , news_photo_count     :: Int
    }
    deriving (Generic, Show)

instance A.ToJSON News
instance FromRow News where
  fromRow =
    News
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> pure (Category 0 "" Nothing)
      <*> liftM pgArrayToList field
      <*> liftM pgArrayToList field
      <*> field
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
  cats <- query_ conn "select id,name,parent from categories;" :: IO [TempCat]
  return $ respJSON $ map (buildAnswer cats) news
 where
  offset = calcOffset (page u) newsPerPage
  limit  = newsPerPage
  t_all  = intListToPGarray <$> tags_all u
  t_any  = intListToPGarray <$> tags_any u

intListToPGarray :: [Int] -> String
intListToPGarray l = T.unpack t2
 where
  t1 = T.intercalate "," $ map (T.pack . show) $ sort l
  t2 = T.concat ["{", t1, "}"]

buildAnswer :: [TempCat] -> News -> News
buildAnswer cats t =
  t { news_category = buildCategories cats $ news_category_id t }

buildCategories :: [TempCat] -> Int -> Category
buildCategories cats c =
  let tempc = head $ filter (\i -> c_id i == c) cats
  in  Category { category_id     = c_id tempc
               , category_name   = c_name tempc
               , category_parent = buildCategories cats <$> c_parent tempc
               }
