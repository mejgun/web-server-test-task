{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetNews where

import qualified Data.Aeson                    as A
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
                                                ( field
                                                , fromRow
                                                )
import           GHC.Generics

import           Lib.DB.Impl.PostgreSQL.Functions
                                                ( pgArrayToList )

data Request =
  Request
    { created_at :: Maybe String
    , created_before :: Maybe String
    , created_after :: Maybe String
    , author_contains :: Maybe String
    , name_contains :: Maybe String
    , text_contains :: Maybe String
    , anything_contains :: Maybe String
    , cat_id :: Maybe Int
    , tags_all :: Maybe [Int]
    , tags_any :: Maybe [Int]
    , sort_by :: Maybe String
    , page :: Int
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Category =
  Category
    { category_id :: Int
    , category_name :: String
    , category_parent :: Maybe Category
    }
  deriving (Generic, Show, Eq)

instance A.ToJSON Category

data News =
  News
    { news_id :: Int
    , news_date :: String
    , news_name :: String
    , news_text :: String
    , news_main_photo :: Maybe String
    , news_author_name :: String
    , news_author_lastname :: String
    , news_category_id :: Int
    , news_category :: Category
    , news_photos :: [String]
    , news_tags :: [String]
    , news_photo_count :: Int
    }
  deriving (Generic, Show, Eq)

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
      <*> fmap pgArrayToList field
      <*> fmap pgArrayToList field
      <*> field
