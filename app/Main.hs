{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

-- import           Blaze.ByteString.Builder       ( fromByteString
--                                                 , fromLazyByteString
--                                                 )
-- import           Blaze.ByteString.Builder.Char.Utf8
--                                                 ( fromShow )
import           Network.HTTP.Types             ( status404 )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )

import           PG
import           Types
import           Users

application :: MyApp
application conn req respond = case pathInfo req of
  ["getusers"  ] -> getUsers conn req respond
  ["createuser"] -> createUser conn req respond
  _              -> respond $ responseBuilder status404 [] err


main :: IO ()
main = do
  conn <- pgconnect
  run 8080 $ logStdout $ application conn


