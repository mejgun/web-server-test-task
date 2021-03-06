{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetNewsComments where

import qualified Data.Aeson                    as A
                                                ( FromJSON
                                                , ToJSON
                                                )
import           Database.PostgreSQL.Simple     ( FromRow )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { news_id :: Int
    , page :: Int
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Comment =
  Comment
    { comment_id :: Int
    , user_name :: String
    , user_lastname :: String
    , comment :: String
    }
  deriving (Generic, Show, Eq)

instance FromRow Comment

instance A.ToJSON Comment
