{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users.GetUsers
  ( getUsers
  )
where
import           Blaze.ByteString.Builder       ( fromLazyByteString )
import qualified Data.Aeson                    as A
import           GHC.Generics
import           Network.HTTP.Types             ( status200 )
import           Network.Wai

import           PG
import           Types

data User = User
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    }
    deriving (Generic, Show)

instance A.ToJSON User
instance A.FromJSON User

getUsers :: MyApp
getUsers conn _ respond = do
  p <-
    query_ conn "select name,lastname,photo from users;" :: IO
      [(String, String, Maybe String)]
  let users =
        map (\(n, l, ph) -> User { name = n, lastname = l, photo = ph }) p
  respond $ responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode
    users
