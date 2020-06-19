{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Categories.DeleteCategory
  ( delete
  )
where

import qualified Data.Aeson                    as A
import           GHC.Generics

import           Lib

data Req = Req
    { cat_id :: Int
    , token  :: String
    }
    deriving (Generic, Show)

instance A.FromJSON Req

delete :: MyHandler Req String
delete conn _ u =
  isAdmin conn (token u)
    >>  ifCategoryExist conn (cat_id u)
    >>  liftIO (execute conn "delete from categories where id=?;" [cat_id u])
    >>= execResult
