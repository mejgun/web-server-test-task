{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetDrafts where

import           Control.Monad                  ( liftM2 )
import qualified Data.Aeson                    as A
import           Data.Maybe                     ( catMaybes )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
                                                ( field
                                                , fromRow
                                                )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )
import           GHC.Generics

data Request =
  Request
    { token :: String
    , page :: Int
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Draft =
  Draft
    { draft_id :: Int
    , draft_date :: String
    , draft_name :: String
    , draft_text :: String
    , draft_main_photo :: Maybe String
    , draft_photos :: [Photo]
    , draft_tags :: [Tag]
    , draft_category_id :: Int
    , draft_category_name :: String
    }
  deriving (Generic, Show)

instance A.ToJSON Draft

instance FromRow Draft where
  fromRow =
    Draft
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> liftM2 (makeR Photo) field field
      <*> liftM2 (makeR Tag)   field field
      <*> field
      <*> field
   where
    makeR
      :: (Int -> String -> a)
      -> PGArray (Maybe Int)
      -> PGArray (Maybe String)
      -> [a]
    makeR c a b = (map (\(x, y) -> c x y)) $ zipPGarrays a b

data Photo =
  Photo
    { photo_id :: Int
    , photo_url :: String
    }
  deriving (Generic, Show)

instance A.ToJSON Photo

data Tag =
  Tag
    { tag_id :: Int
    , tag_name :: String
    }
  deriving (Generic, Show)

instance A.ToJSON Tag

zipPGarrays :: PGArray (Maybe a) -> PGArray (Maybe b) -> [(a, b)]
zipPGarrays a1 a2 = zip (pgArrayToList a1) (pgArrayToList a2)

pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList = catMaybes . fromPGArray
