{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.GetDrafts
  ( getDrafts
  )
where


import           Control.Monad                  ( liftM2 )
import qualified Data.Aeson                    as A
import           Data.Maybe                     ( catMaybes )
import           Database.PostgreSQL.Simple.FromRow
                                                ( field
                                                , fromRow
                                                )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..) )
import           GHC.Generics
import           PG
import           Types

data Draft = Draft
    { draft_id            :: Int
    , draft_date          :: String
    , draft_name          :: String
    , draft_text          :: String
    , draft_main_photo    :: Maybe String
    , draft_photos        :: [Photo]
    , draft_tags          :: [Tag]
    , draft_category_id   :: Int
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
      <*> liftM2 makePhoto field field
      <*> liftM2 makeTag   field field
      <*> field
      <*> field
   where
    makePhoto :: PGArray (Maybe Int) -> PGArray (Maybe String) -> [Photo]
    makePhoto a b = map (\(x, y) -> Photo x y) $ zipPGarrays a b
    makeTag :: PGArray (Maybe Int) -> PGArray (Maybe String) -> [Tag]
    makeTag a b = map (\(x, y) -> Tag x y) $ zipPGarrays a b

data Photo = Photo
    { photo_id  :: Int
    , photo_url :: String
    }
    deriving (Generic, Show)

instance A.ToJSON Photo

data Tag = Tag
    { tag_id   :: Int
    , tag_name :: String
    }
    deriving (Generic, Show)

instance A.ToJSON Tag

data Req = Req
    { token :: String
    , page  :: Int
    }
    deriving (Generic, Show)

instance A.FromJSON Req

getDrafts :: MyHandler Req
getDrafts conn _ = handleSqlErr $ do
  drafts <-
    query_
      conn
      "select n.id,n.date::text,n.name,n.text,n.main_photo,array_agg(np.id),array_agg(np.photo),array_agg(nt.tag_id),array_agg(t.name),n.category_id,c.name from news as n left join news_photos as np on n.id=np.news_id left join news_tags as nt on nt.news_id=n.id left join tags as t on t.id=nt.tag_id left join categories as c on c.id=n.category_id group by n.id, c.name;" :: IO
      [Draft]
  return $ respJSON drafts

zipPGarrays :: PGArray (Maybe a) -> PGArray (Maybe b) -> [(a, b)]
zipPGarrays a1 a2 = zip (pgArrayToList a1) (pgArrayToList a2)

pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList p = catMaybes $ fromPGArray p
