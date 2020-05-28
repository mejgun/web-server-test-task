{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyApp
  , MyHandler
  , responseOK
  , responseERR
  , responseSQLERR
  , respJSON
  , jsonCT
  , handleSqlErr
  , usersPerPage
  , authorsPerPage
  , categoriesPerPage
  , tagsPerPage
  , rIfAdmin
  , rIfJsonBody
  , rJSON
  , module Control.Exception
  )
where

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                , fromLazyByteString
                                                )
import           Control.Exception              ( handle )
import           Data.Aeson                    as A
import qualified Data.ByteString               as B
import           Network.HTTP.Types             ( HeaderName
                                                , status200
                                                , status404
                                                , status409
                                                )
import           Network.Wai

import           PG

type MyApp
  =  Connection
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived

type MyHandler a
  = Connection -> (Response -> IO ResponseReceived) -> a -> IO ResponseReceived

usersPerPage :: Int
usersPerPage = 10

authorsPerPage :: Int
authorsPerPage = 5

categoriesPerPage :: Int
categoriesPerPage = 15

tagsPerPage :: Int
tagsPerPage = 20

isAdmin :: Connection -> String -> IO Bool
isAdmin conn token = do
  p <- query conn "select admin from users where token = ? limit 1;" [token]
  return $ case p of
    [Only i] -> i
    _        -> False

ok :: Builder
ok = fromByteString "{\"ok\":\"ok\"}"

responseOK :: Response
responseOK = responseBuilder status200 jsonCT ok

err :: Builder
err = fromByteString "{\"error\":\"error\"}"

responseERR :: Response
responseERR = responseBuilder status404 [] ""

responseSQLERR :: Response
responseSQLERR = responseBuilder status409 jsonCT err

respJSON :: (A.ToJSON a) => a -> Response
respJSON j = responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j

jsonCT :: [(HeaderName, B.ByteString)]
jsonCT = [("Content-Type", "application/json")]

handleSqlErr
  :: (Response -> IO ResponseReceived)
  -> IO ResponseReceived
  -> IO ResponseReceived
handleSqlErr r = handle (checkSqlErr (r responseSQLERR))
 where
  checkSqlErr :: IO ResponseReceived -> SqlError -> IO ResponseReceived
  checkSqlErr x e = print e >> x

bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
bodyToJSON x = A.decode <$> lazyRequestBody x

rIfJsonBody :: A.FromJSON a => MyHandler a -> MyApp
rIfJsonBody x conn req respond =
  bodyToJSON req >>= maybe (respond responseERR) (x conn respond)

rIfAdmin
  :: Connection
  -> (Response -> IO ResponseReceived)
  -> String
  -> IO ResponseReceived
  -> IO ResponseReceived
rIfAdmin conn respond token r = do
  adm <- isAdmin conn token
  if adm then r else respond responseERR

rJSON
  :: (A.ToJSON a, FromRow a)
  => (Response -> IO ResponseReceived)
  -> IO [a]
  -> IO ResponseReceived
rJSON respond q = q >>= respond . respJSON
