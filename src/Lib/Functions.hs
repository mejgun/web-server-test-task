{-# LANGUAGE OverloadedStrings #-}

module Lib.Functions
  ( returnFile
  , return404
  , isAdmin
  , isAuthor
  , ifLoginExist
  , ifLoginNotExist
  , execResult
  , isValidPage
  , ifCategoryExist
  , ifTagNotExist
  , ifTagExist
  , ifNewsExist
  , ifNewsPublished
  , ifNewsAuthor
  , isUser
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

import           Control.Monad.Except
import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )


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

isAdmin :: Connection -> String -> HandlerMonad Bool
isAdmin conn token = rIfDB
  conn
  (Query "select admin from users where token=? limit 1;")
  [token]
  Error404

isAuthor :: Connection -> String -> HandlerMonad Bool
isAuthor c token = rIfDB
  c
  "select count(id)=1 from authors where user_id=(select id from users where token=?);"
  [token]
  ErrorNotAuthor

isUser :: Connection -> String -> HandlerMonad Bool
isUser conn token = rIfDB
  conn
  (Query "select count(id)=1 from users where token=?;")
  [token]
  ErrorNotUser

ifLoginExist :: Connection -> String -> HandlerMonad Bool
ifLoginExist c login = ifLogin 1 c login ErrorLoginNotExist

ifLoginNotExist :: Connection -> String -> HandlerMonad Bool
ifLoginNotExist c login = ifLogin 0 c login ErrorLoginAlreadyExist

ifLogin
  :: Int -> Connection -> String -> ResultResponseError -> HandlerMonad Bool
ifLogin cond c login rElse =
  rIfDB c "select count(id)=? from users where login=?;" (cond, login) rElse

ifCategoryExist :: Connection -> Int -> HandlerMonad Bool
ifCategoryExist c cat = rIfDB
  c
  "select count(id)=1 from categories where id=?;"
  [cat]
  ErrorCategoryNotExist

ifTagNotExist :: Connection -> String -> HandlerMonad Bool
ifTagNotExist c tag = rIfDB c
                            "select count(id)=0 from tags where name=?;"
                            [tag]
                            ErrorTagAlreadyExist

ifTagExist :: Connection -> Int -> HandlerMonad Bool
ifTagExist c tag_id =
  rIfDB c "select count(id)=1 from tags where id=?;" [tag_id] ErrorTagNotExist

ifNewsExist :: Connection -> Int -> HandlerMonad Bool
ifNewsExist c news_id =
  rIfDB c "select count(id)=1 from news where id=?;" [news_id] ErrorNewsNotExist

ifNewsPublished :: Connection -> Int -> HandlerMonad Bool
ifNewsPublished c news_id = rIfDB
  c
  "select count(id)=1 from news where id=? and published=true;"
  [news_id]
  ErrorNewsNotExist

ifNewsAuthor :: Connection -> Int -> String -> HandlerMonad Bool
ifNewsAuthor c news_id token = rIfDB
  c
  "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
  (news_id, token)
  ErrorNotYourNews

isValidPage :: Int -> HandlerMonad Bool
isValidPage p = if p > 0 then return True else throwError ErrorBadPage

execResult :: GHC.Int.Int64 -> HandlerMonad String
execResult i = case i of
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
