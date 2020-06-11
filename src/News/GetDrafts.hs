{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.GetDrafts
  ( getDrafts
  )
where


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
    { draft_id         :: Int
    , draft_date       :: String
    , draft_name       :: String
    , draft_text       :: String
    , draft_main_photo :: Maybe String
    , draft_photos     :: [Photo]
    , draft_tags       :: [Tag]
    }
    deriving (Generic, Show)

instance A.ToJSON Draft
instance FromRow Draft where
  fromRow = do
    d1 <- field
    d2 <- field
    d3 <- field
    d4 <- field
    d5 <- field
    d6 <- field
    d7 <- field
    d8 <- field
    d9 <- field
    return Draft
      { draft_id         = d1
      , draft_date       = d2
      , draft_name       = d3
      , draft_text       = d4
      , draft_main_photo = d5
      , draft_photos = map (\(x, y) -> Photo { photo_id = x, photo_url = y })
                         $ zipPGarrays d6 d7
      , draft_tags       = map (\(x, y) -> Tag { tag_id = x, tag_name = y })
                             $ zipPGarrays d8 d9
      }

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
      "select n.id,n.date::text,n.name,n.text,n.main_photo,array_agg(np.id) as photo_id,array_agg(np.photo) as photo,array_agg(nt.tag_id) as tag_id,array_agg(t.name) as tag_name from news as n left join news_photos as np on n.id=np.news_id left join news_tags as nt on nt.news_id=n.id left join tags as t on t.id=nt.tag_id group by n.id;" :: IO
      [Draft]
  return $ respJSON drafts

zipPGarrays :: PGArray (Maybe a) -> PGArray (Maybe b) -> [(a, b)]
zipPGarrays a1 a2 = zip (pgArrayToList a1) (pgArrayToList a2)

pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList p = catMaybes $ fromPGArray p
