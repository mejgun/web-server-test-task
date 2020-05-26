{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyApp
  , responseOK
  , responseERR
  , isAdmin
  , jsonCT
  )
where

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                )
import qualified Data.ByteString               as B
import           Network.HTTP.Types             ( HeaderName
                                                , status200
                                                , status404
                                                )
import           Network.Wai

import           PG

type MyApp
  =  Connection
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived

ok :: Builder
ok = fromByteString "{\"ok\":\"ok\"}"

err :: Builder
err = fromByteString "{\"error\":\"error\"}"

isAdmin :: Connection -> String -> IO Bool
isAdmin conn token = do
  p <- query conn "select admin from users where token = ?" [token]
  return $ case p of
    [Only i] -> i
    _        -> False

responseOK :: Response
responseOK = responseBuilder status200 jsonCT ok

responseERR :: Response
responseERR = responseBuilder status404 jsonCT err

jsonCT :: [(HeaderName, B.ByteString)]
jsonCT = [("Content-Type", "application/json")]
