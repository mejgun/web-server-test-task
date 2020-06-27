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
                                                ( ByteString
                                                , writeFile
                                                )
import qualified Data.ByteString.Char8         as B8
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..)
                                                , Query(..)
                                                )
import qualified GHC.Int                        ( Int64 )
import           Network.HTTP.Types             ( HeaderName
                                                , status200
                                                , status400
                                                , status404
                                                , status500
                                                )
import           Network.Wai
import           System.Directory               ( doesFileExist )

import           Lib.Constants
import qualified Lib.Logger                    as Logger
import qualified Lib.Logic                     as Logic
import           Lib.Types

rIfJsonBody
  :: (FromJSON a, ToJSON b, Show b)
  => Logic.ResultResponseError
  -> Logic.MyHandler a b
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
rIfJsonBody rs x req respond = do
  j <- bodyToJSON req
  q <- maybe (throw rs) x j
  respond $ eitherToResponse q
 where
  bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
  bodyToJSON j = A.decode <$> lazyRequestBody j

normalHandler
  :: (A.FromJSON a, A.ToJSON b, Show b)
  => Logic.MyHandler a b
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
normalHandler = rIfJsonBody Logic.ErrorBadRequest

adminHandler
  :: (A.FromJSON a, A.ToJSON b, Show b)
  => Logic.MyHandler a b
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
adminHandler = rIfJsonBody Logic.ErrorNotFound

eitherToResponse :: A.ToJSON b => Logic.ResultResponse b -> Response
eitherToResponse r = case r of
  Logic.Success j ->
    responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j
  Logic.Error Logic.ErrorNotFound -> responseBuilder status404 [] "Not Found"
  Logic.Error Logic.ErrorInternal ->
    responseBuilder status500 [] "Internal Server Error"
  Logic.Error p -> e p
 where
  e :: Logic.ResultResponseError -> Response
  e = responseBuilder status400 jsonCT . toErr . show
  toErr :: String -> Builder
  toErr s = fromByteString $ B8.pack $ "{\"error\":\"" ++ s ++ "\"}"
  jsonCT :: [(HeaderName, B.ByteString)]
  jsonCT = [("Content-Type", "application/json")]

return404 :: (Response -> IO ResponseReceived) -> IO ResponseReceived
return404 rd = rd $ responseBuilder status404 [] ""

returnFile :: T.Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
returnFile f rd = do
  let file = imagesDir ++ (T.unpack f)
  exist <- doesFileExist file
  rd $ case exist of
    True -> do
      responseFile status200 contentType file Nothing
    _ -> responseBuilder status404 [] ""
 where
  contentType :: [(HeaderName, B.ByteString)]
  contentType = case T.split (== '.') f of
    [] -> []
    l ->
      let ext = encodeUtf8 $ T.toLower $ last l
      in  [("Content-Type", "image/" <> ext)]
