{-# LANGUAGE DeriveGeneric #-}

module Lib.Types.GetAuthors where

import qualified Data.Aeson                    as A
                                                ( FromJSON
                                                , ToJSON
                                                )
import           Database.PostgreSQL.Simple     ( FromRow )
import           GHC.Generics                   ( Generic )

data Request =
  Request
    { token :: String
    , page :: Int
    }
  deriving (Generic, Show)

instance A.FromJSON Request

data Author =
  Author
    { name :: String
    , lastname :: String
    , photo :: Maybe String
    , descr :: String
    }
  deriving (Generic, Show, Eq)

instance FromRow Author

instance A.ToJSON Author
