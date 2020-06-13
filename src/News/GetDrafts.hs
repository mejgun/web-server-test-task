{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.GetDrafts
  ( getDrafts
  )
where


import           Control.Monad                  ( liftM2 )
import qualified Data.Aeson                    as A
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
getDrafts conn u =
  rIfAuthor conn (token u)
    $   handleSqlErr
    $   respJSON
    <$> (query
          conn
          "select n.id,n.date::text,n.name,n.text,n.main_photo,array_agg(np.id),array_agg(np.photo),array_agg(nt.tag_id),array_agg(t.name),n.category_id,c.name from news as n left join news_photos as np on n.id=np.news_id left join news_tags as nt on nt.news_id=n.id left join tags as t on t.id=nt.tag_id left join categories as c on c.id=n.category_id left join authors as a on n.author_id=a.id left join users as u on a.user_id=u.id where u.token=? and n.published=false group by n.id, c.name offset ? limit ?;"
          (token u, offset, limit) :: IO [Draft]
        )
 where
  offset = calcOffset (page u) newsPerPage
  limit  = newsPerPage


zipPGarrays :: PGArray (Maybe a) -> PGArray (Maybe b) -> [(a, b)]
zipPGarrays a1 a2 = zip (pgArrayToList a1) (pgArrayToList a2)


