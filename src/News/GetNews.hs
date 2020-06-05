{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module News.GetNews
  ( get
  )
where


import qualified Data.Aeson                    as A
import           GHC.Generics

-- import           PG
import           Types

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
    , sort              :: Maybe String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

get :: MyHandler Req
get _ _ = undefined
