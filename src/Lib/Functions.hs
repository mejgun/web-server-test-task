{-# LANGUAGE OverloadedStrings #-}

module Lib.Functions
  ( returnFile
  , return404
  , rIfAdmin
  , rIfAuthor
  , rIfLoginExist
  , rIfLoginNotExist
  , rIfAuthorExist
  , rExecResult
  , rIfValidPage
  , rIfCategoryExist
  , rIfTagNotExist
  , rIfTagExist
  , rIfNewsExist
  , rIfNewsPublished
  , rIfNewsAuthor
  , rIfUser
  , createImagesDir
  , pgArrayToList
  , calcOffset
  , normalHandler
  , adminHandler
  , readConfig
  , module Database.PostgreSQL.Simple
  , module Control.Monad.Except
  )
where

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                , fromLazyByteString
                                                )
-- import           Control.Exception              ( handle )
-- import           Control.Monad                  ( when )
import           Data.Aeson                    as A
import qualified Data.ByteString               as B
                                                ( ByteString )
import qualified Data.ByteString.Char8         as B8
import           Data.Maybe                     ( fromJust )
import           Data.Maybe                     ( catMaybes )
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
                                                )
import           Network.Wai
import           System.IO                      ( IOMode(..)
                                                , hPutStrLn
                                                , openFile
                                                )

import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )
import           Control.Monad.Except


import           Lib.Constants
import           Lib.Types

readConfig :: IO Config
readConfig = do
  j <- fromJust <$> A.decodeFileStrict "config.json" :: IO Conf
  c <- connectPostgreSQL $ B8.pack $ pgconfig j
  h <- openFile "app.log" AppendMode
  let lgLvl = strToLogLevel $ log_level j
  return Config { connection = c
                , hnd        = h
                , logger     = logg h lgLvl
                , loglevel   = lgLvl
                }
 where
  logg :: AppLogger
  logg h appLogLvl msgLogLvl s =
    if appLogLvl <= msgLogLvl then hPutStrLn h s else return ()

  strToLogLevel :: String -> LogLevel
  strToLogLevel s = case s of
    "quiet"  -> LogQuiet
    "normal" -> LogNormal
    _        -> LogDebug

-- handleSqlErr
--   :: A.ToJSON a => Logger -> IO (ResultResponse a) -> IO (ResultResponse a)
-- handleSqlErr logg = handle $ checkSqlErr $ return ErrorBadRequest
--  where
--   checkSqlErr
--     :: A.ToJSON a => IO (ResultResponse a) -> SqlError -> IO (ResultResponse a)
--   checkSqlErr x e = printErr e >> x

--   printErr :: SqlError -> IO ()
--   printErr (SqlError q w t e r) = logg LogQuiet $ B8.unpack $ B8.intercalate
--     " "
--     [q, B8.pack (show w), e, r, t]

rIfJsonBody
  :: (FromJSON a, ToJSON b, Show b)
  => ResultResponseError
  -> MyHandler a b
  -> MyApp
rIfJsonBody rs x conn logg req respond = do
  j <- bodyToJSON req
  q <- runExceptT $ maybe (throwError rs) {-(handleSqlErr (logg) . (x conn logg))-}
                                          (x conn logg) j
  respond $ eitherToResponse q
 where
  bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
  bodyToJSON j = A.decode <$> lazyRequestBody j

normalHandler :: (A.FromJSON a, A.ToJSON b, Show b) => MyHandler a b -> MyApp
normalHandler = rIfJsonBody ErrorBadRequest

adminHandler :: (A.FromJSON a, A.ToJSON b, Show b) => MyHandler a b -> MyApp
adminHandler = rIfJsonBody Error404

eitherToResponse :: A.ToJSON b => Either ResultResponseError b -> Response
eitherToResponse r = case r of
  Right j -> responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j
  Left Error404 -> responseBuilder status404 [] ""
  Left p -> e p
 where
  e :: ResultResponseError -> Response
  e = responseBuilder status400 jsonCT . toErr . show

  toErr :: String -> Builder
  toErr s = fromByteString $ B8.pack $ "{\"error\":\"" ++ s ++ "\"}"

  jsonCT :: [(HeaderName, B.ByteString)]
  jsonCT = [("Content-Type", "application/json")]

rIfDB
  :: ToRow a
  => Connection
  -> Query
  -> a
  -> ResultResponseError
  -> HandlerMonad Bool
rIfDB c q val rElse = do
  p <- liftIO (query c q val :: IO [Only Bool])
  case p of
    [Only True] -> return True
    _           -> throwError rElse

rIfAdmin :: Connection -> String -> HandlerMonad Bool
rIfAdmin conn token = rIfDB
  conn
  (Query "select admin from users where token=? limit 1;")
  [token]
  Error404

rIfAuthor :: Connection -> String -> HandlerMonad Bool
rIfAuthor c token = rIfDB
  c
  "select count(id)=1 from authors where user_id=(select id from users where token=?);"
  [token]
  ErrorNotAuthor

rIfUser :: Connection -> String -> HandlerMonad Bool
rIfUser conn token = rIfDB
  conn
  (Query "select count(id)=1 from users where token=?;")
  [token]
  ErrorNotUser

rIfLoginExist :: Connection -> String -> HandlerMonad Bool
rIfLoginExist c login = rIfLogin 1 c login ErrorLoginNotExist

rIfLoginNotExist :: Connection -> String -> HandlerMonad Bool
rIfLoginNotExist c login = rIfLogin 0 c login ErrorLoginAlreadyExist

rIfLogin
  :: Int -> Connection -> String -> ResultResponseError -> HandlerMonad Bool
rIfLogin cond c login rElse =
  rIfDB c "select count(id)=? from users where login=?;" (cond, login) rElse

rIfAuthorExist :: Connection -> String -> HandlerMonad Bool
rIfAuthorExist c login = rIfDB
  c
  "select count(id)=1 from authors where user_id=(select id from users where login=?);"
  [login]
  ErrorAuthorNotExist

rIfCategoryExist :: Connection -> Int -> HandlerMonad Bool
rIfCategoryExist c cat = rIfDB
  c
  "select count(id)=1 from categories where id=?;"
  [cat]
  ErrorCategoryNotExist

rIfTagNotExist :: Connection -> String -> HandlerMonad Bool
rIfTagNotExist c tag = rIfDB c
                             "select count(id)=0 from tags where name=?;"
                             [tag]
                             ErrorTagAlreadyExist

rIfTagExist :: Connection -> Int -> HandlerMonad Bool
rIfTagExist c tag_id =
  rIfDB c "select count(id)=1 from tags where id=?;" [tag_id] ErrorTagNotExist

rIfNewsExist :: Connection -> Int -> HandlerMonad Bool
rIfNewsExist c news_id =
  rIfDB c "select count(id)=1 from news where id=?;" [news_id] ErrorNewsNotExist

rIfNewsPublished :: Connection -> Int -> HandlerMonad Bool
rIfNewsPublished c news_id = rIfDB
  c
  "select count(id)=1 from news where id=? and published=true;"
  [news_id]
  ErrorNewsNotExist

rIfNewsAuthor :: Connection -> Int -> String -> HandlerMonad Bool
rIfNewsAuthor c news_id token = rIfDB
  c
  "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
  (news_id, token)
  ErrorNotYourNews

rIfValidPage :: Int -> HandlerMonad Bool
rIfValidPage p = if p > 0 then return True else throwError ErrorBadPage

rExecResult :: GHC.Int.Int64 -> HandlerMonad String
rExecResult i = case i of
  1 -> return ok
  _ -> throwError ErrorBadRequest

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
      in  [("Content-Type", "application/" <> ext)]

createImagesDir :: IO ()
createImagesDir = doesDirectoryExist imagesDir
  >>= \exist -> when (not exist) $ createDirectory imagesDir

pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList p = catMaybes $ fromPGArray p

calcOffset :: Int -> Int -> Int
calcOffset page perpage = (page - 1) * perpage
