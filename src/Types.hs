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
  , commentsPerPage
  , imagesDir
  , rIfAdmin
  , rIfAuthor
  , rIfJsonBody
  )
where

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                , fromLazyByteString
                                                )
import           Control.Exception              ( handle )
import           Data.Aeson                    as A
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
                                                ( putStrLn )
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

type MyHandler a = Connection -> a -> IO Response

usersPerPage :: Int
usersPerPage = 10

authorsPerPage :: Int
authorsPerPage = 5

categoriesPerPage :: Int
categoriesPerPage = 15

tagsPerPage :: Int
tagsPerPage = 20

commentsPerPage :: Int
commentsPerPage = 10

imagesDir :: String
imagesDir = "images/"

isAdmin :: Connection -> String -> IO Bool
isAdmin conn token = do
  p <- query conn "select admin from users where token = ? limit 1;" [token]
  return $ case p of
    [Only i] -> i
    _        -> False

isAuthor :: Connection -> String -> IO Bool
isAuthor conn token = do
  p <- query
    conn
    "select count(id)=1 from authors where user_id=(select id from users where token=?);"
    [token]
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

handleSqlErr :: IO Response -> IO Response
handleSqlErr = handle $ checkSqlErr $ return responseSQLERR
 where
  checkSqlErr :: IO Response -> SqlError -> IO Response
  checkSqlErr x e = printErr e >> x
  printErr :: SqlError -> IO ()
  printErr (SqlError q w t e r) = print w >> mapM_ B8.putStrLn [q, e, r, t]

bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
bodyToJSON x = A.decode <$> lazyRequestBody x

rIfJsonBody :: A.FromJSON a => MyHandler a -> MyApp
rIfJsonBody x conn req respond = do
  j <- bodyToJSON req
  q <- maybe (return responseERR) (x conn) j
  respond q

rIfAdmin :: Connection -> String -> IO Response -> IO Response
rIfAdmin c t r = responseIf isAdmin c t r responseERR

rIfAuthor :: Connection -> String -> IO Response -> IO Response
rIfAuthor c t r = responseIf isAuthor c t r responseSQLERR

responseIf
  :: (Connection -> String -> IO Bool)
  -> Connection
  -> String
  -> IO Response
  -> Response
  -> IO Response
responseIf cond conn token r rElse = do
  a <- cond conn token
  if a then r else return rElse
