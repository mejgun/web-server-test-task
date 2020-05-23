{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai
import           Network.HTTP.Types             ( status200 )
import           Network.Wai.Handler.Warp       ( run )

import           Blaze.ByteString.Builder       ( fromByteString )
import           Blaze.ByteString.Builder.Char.Utf8
                                                ( fromShow )
import           Control.Concurrent.MVar
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )
--import           Data.Monoid                    ( (<>) )

--import           Lib

application
  :: MVar Int
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
application countRef req respond = do
  modifyMVar countRef $ \count -> do
    let count' = count + 1
        msg =
          fromByteString "You are visitor number: "
            <> fromShow count'
            <> fromShow (queryString req)
            <> fromShow (pathInfo req)
    responseReceived <- respond
      $ responseBuilder status200 [("Content-Type", "text/plain")] msg
    return (count', responseReceived)

main :: IO ()
main = do
  visitorCount <- newMVar 0
  run 8080 $ logStdout $ application visitorCount
