{-# LANGUAGE OverloadedStrings #-}

module Lib.Functions
  ( returnFile
  , return404
  , normalHandler
  , adminHandler
  )
where

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                , fromLazyByteString
                                                )
import           Control.Exception
import           Data.Aeson                    as A
import qualified Data.ByteString               as B
                                                ( ByteString )
import qualified Data.ByteString.Char8         as B8
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Network.HTTP.Types             ( HeaderName
                                                , status200
                                                , status400
                                                , status404
                                                , status500
                                                )
import           Network.Wai
import           System.Directory               ( doesFileExist )

import           Lib.Constants
import qualified Lib.Handlers                  as Handlers

rIfJsonBody
  :: (FromJSON a, ToJSON b, Show b)
  => Handlers.ResultResponseError
  -> (a -> Handlers.Result b)
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
rIfJsonBody rError handler jsonReq respond = do
  request <- bodyToJSON jsonReq
  result  <- try $ maybe (throw rError) handler request
  respond $ eitherToResponse result
 where
  bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
  bodyToJSON j = A.decode <$> lazyRequestBody j

normalHandler
  :: (A.FromJSON a, A.ToJSON b, Show b)
  => (a -> Handlers.Result b)
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
normalHandler = rIfJsonBody Handlers.ErrorBadRequest

adminHandler
  :: (A.FromJSON a, A.ToJSON b, Show b)
  => (a -> Handlers.Result b)
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
adminHandler = rIfJsonBody Handlers.ErrorNotFound

eitherToResponse
  :: A.ToJSON b => Either Handlers.ResultResponseError b -> Response
eitherToResponse r = case r of
  Right res ->
    responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode res
  Left Handlers.ErrorNotFound -> responseBuilder status404 [] "Not Found"
  Left Handlers.ErrorInternal ->
    responseBuilder status500 [] "Internal Server Error"
  Left errRes -> e errRes
 where
  e :: Handlers.ResultResponseError -> Response
  e = responseBuilder status400 jsonCT . toErr . show
  toErr :: String -> Builder
  toErr s = fromByteString $ B8.pack $ "{\"error\":\"" ++ s ++ "\"}"
  jsonCT :: [(HeaderName, B.ByteString)]
  jsonCT = [("Content-Type", "application/json")]

return404 :: (Response -> IO ResponseReceived) -> IO ResponseReceived
return404 respond = respond $ responseBuilder status404 [] ""

returnFile :: T.Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
returnFile fileName respond = do
  let file = imagesDir <> T.unpack fileName
  exist <- doesFileExist file
  respond $ if exist
    then responseFile status200 contentType file Nothing
    else responseBuilder status404 [] ""
 where
  contentType :: [(HeaderName, B.ByteString)]
  contentType = case T.split (== '.') fileName of
    [] -> []
    list ->
      let ext = encodeUtf8 $ T.toLower $ last list
      in  [("Content-Type", "image/" <> ext)]
