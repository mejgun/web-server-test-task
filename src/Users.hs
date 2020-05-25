{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Users
  ( getUsers
  , createUser
  )
where
-- import           Blaze.ByteString.Builder       ( fromByteString
--                                                 , fromLazyByteString
--                                                 )
import           Blaze.ByteString.Builder.Char.Utf8
                                                ( fromShow )
import qualified Data.Aeson                    as A
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics
import           Network.HTTP.Types             ( status200
                                                , status404
                                                )
import           Network.Wai


import           PG
import           Types

-- isAdmin :: Connection -> String -> IO Bool
-- isAdmin conn token = do
--   p <- query conn "select admin from users where token = ?" [token]
--   return $ case p of
--     [Only i] -> i
--     _        -> False

getUsers :: MyApp
getUsers conn _ respond = do
  p <-
    query_ conn "select name,lastname,photo from users;" :: IO
      [(String, String, Maybe String)]
  respond $ responseBuilder status200 [] $ fromShow p

data CreateUser = CreateUser
    { name     :: String
    , lastname :: String
    , photo    :: Maybe String
    , login    :: String
    , password :: String
    }
    deriving (Generic, Show)

instance A.ToJSON CreateUser
instance A.FromJSON CreateUser

createUser :: MyApp
createUser conn req respond = do
  b <- lazyRequestBody req
  let p = A.decode b :: Maybe CreateUser
  case p of
    Just u -> do
      let img = fromMaybe "" (photo u)
      _ <- execute
        conn
        "insert into users (name,lastname,photo,token,login,password) values(?,?,?,md5(random()::text),?,md5(?));"
        [name u, lastname u, img, login u, password u]
      respond $ responseBuilder status200 [] ok
    _ -> respond $ responseBuilder status404 [] err
