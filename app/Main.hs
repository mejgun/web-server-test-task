{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Blaze.ByteString.Builder       ( fromByteString )
import           Blaze.ByteString.Builder.Char.Utf8
                                                ( fromShow )
import           Network.HTTP.Types             ( status200 )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )

import           PG
application
  :: Connection
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
application conn req respond = case pathInfo req of
  ["getusers"] -> getUsers conn req respond
  _            -> do
    let msg =
          fromByteString "You are visitor number: "
            <> fromShow (queryString req)
            <> fromShow (pathInfo req)
    respond $ responseBuilder status200 [("Content-Type", "text/plain")] msg


main :: IO ()
main = do
  conn <- pgconnect
  print =<< isAdmin conn "97b0febcad13268a5a12de9d09436ab5"
  run 8080 $ logStdout $ application conn


isAdmin :: Connection -> String -> IO Bool
isAdmin conn token = do
  p <- query conn "select admin from users where token = ?" [token]
  return $ case p of
    [Only i] -> i
    _        -> False

getUsers
  :: Connection
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
getUsers conn req respond = do
  p <-
    query_ conn "select name,lastname,photo from users;" :: IO
      [(String, String, Maybe String)]
  respond $ responseBuilder status200 [] $ fromShow p
