{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyApp
  , ok
  , err
  )
where

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                )
import           Network.Wai

import           PG

type MyApp
  =  Connection
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived

ok :: Builder
ok = fromByteString "OK\n"

err :: Builder
err = fromByteString "ERR\n"
