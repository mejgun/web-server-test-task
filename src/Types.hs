{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyApp
  , responseOK
  , responseERR
  , responseSQLERR
  , responseJSON
  , isAdmin
  , jsonCT
  , checkSqlErr
  , bodyToJSON
  , usersPerPage
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

usersPerPage :: Int
usersPerPage = 10

isAdmin :: Connection -> String -> IO Bool
isAdmin conn token = do
  p <- query conn "select admin from users where token = ?" [token]
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

responseJSON :: (A.ToJSON a) => a -> Response
responseJSON j =
  responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j


jsonCT :: [(HeaderName, B.ByteString)]
jsonCT = [("Content-Type", "application/json")]

checkSqlErr :: IO ResponseReceived -> SqlError -> IO ResponseReceived
-- checkSqlErr x (SqlError _ _ _ _ _) = x
checkSqlErr x e = print e >> x

bodyToJSON :: (A.FromJSON a) => Request -> IO (Maybe a)
bodyToJSON x = A.decode <$> lazyRequestBody x
