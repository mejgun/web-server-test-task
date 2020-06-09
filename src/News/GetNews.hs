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

data TempPhoto = TempPhoto
    { p_n_id :: Int
    , p_path :: String
    }
    deriving (Generic, Show)

instance FromRow TempPhoto

data Category = Category
    { category_id     :: Int
    , category_name   :: String
    , category_parent :: Maybe Category
    }
    deriving (Generic, Show)

instance A.ToJSON Category

data Tag = Tag
    { tag_id   :: Int
    , tag_name :: String
    }
    deriving (Generic, Show)

instance A.ToJSON Tag

data News = News
    { news_id              :: Int
    , news_date            :: String
    , news_name            :: String
    , news_text            :: String
    , news_main_photo      :: Maybe String
    , news_author_name     :: String
    , news_author_lastname :: String
    , news_category        :: Category
    , news_photos          :: [String]
    , news_tags            :: [Tag]
    }
    deriving (Generic, Show)

instance A.ToJSON News

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
  tags <-
    query
      conn
      "select nt.news_id,nt.tag_id,t.name from news_tags as nt left join tags as t on t.id=nt.tag_id where nt.news_id in ?;"
    $ Only
    $ In
    $ map n_id news :: IO [TempTag]
  cats   <- query_ conn "select id,name,parent from categories;" :: IO [TempCat]
  photos <-
    query conn "select news_id,photo from news_photos where news_id in ?;"
    $ Only
    $ In
    $ map n_id news :: IO [TempPhoto]
  return $ respJSON $ map (buildAnswer photos tags cats) news
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

buildCategories :: [TempCat] -> Int -> Category
buildCategories cats c =
  let tempc = head $ filter (\i -> c_id i == c) cats
  in  Category { category_id     = c_id tempc
               , category_name   = c_name tempc
               , category_parent = buildCategories cats <$> c_parent tempc
               }

buildTags :: [TempTag] -> Int -> [Tag]
buildTags t i = mp
 where
  flt = filter (\x -> t_n_id x == i) t
  mp  = map (\x -> Tag { tag_id = t_id x, tag_name = t_name x }) flt

buildPhotos :: [TempPhoto] -> Int -> [String]
buildPhotos p i = map p_path $ filter (\x -> p_n_id x == i) p

buildAnswer :: [TempPhoto] -> [TempTag] -> [TempCat] -> TempNews -> News
buildAnswer photos tags cats t = News
  { news_id              = n_id t
  , news_date            = show $ n_date t
  , news_name            = n_name t
  , news_text            = n_text t
  , news_main_photo      = n_main_photo t
  , news_author_name     = n_author_name t
  , news_author_lastname = n_author_lastname t
  , news_category        = buildCategories cats $ n_category_id t
  , news_photos          = buildPhotos photos $ n_id t
  , news_tags            = buildTags tags $ n_id t
  }

