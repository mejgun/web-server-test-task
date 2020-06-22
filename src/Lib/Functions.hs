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
  , ifAuthorExist
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
  , deleteFile
  , saveFile
  , makeExt
  , decodeBase64
  , module Database.PostgreSQL.Simple
  , module Control.Exception
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
import           Data.ByteString.Base64         ( decodeLenient )
import qualified Data.ByteString.Char8         as B8
import           Data.ByteString.UTF8           ( fromString )
import           Data.Char                      ( toLower )
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
                                                , status500
                                                )
import           Network.Wai
import           System.Directory               ( createDirectoryIfMissing
                                                , doesFileExist
                                                , getPermissions
                                                , readable
                                                , removeFile
                                                , writable
                                                )
import           System.IO                      ( IOMode(..)
                                                , hFlush
                                                , hPutStrLn
                                                , openFile
                                                )


import           Lib.Constants
import           Lib.Types

readConfig :: FilePath -> IO Config
readConfig f = do
  j <- fromJust <$> A.decodeFileStrict f :: IO Conf
  c <- connectPostgreSQL $ B8.pack $ pgconfig j
  h <- openFile (log_file j) AppendMode
  let lgLvl = strToLogLevel $ log_level j
  return Config { connection = c
                , hnd        = h
                , logger     = logg h lgLvl
                , loglevel   = lgLvl
                }
 where
  logg :: AppLogger
  logg h appLogLvl msgLogLvl s =
    if appLogLvl <= msgLogLvl then hPutStrLn h s >> hFlush h else return ()

  strToLogLevel :: String -> LogLevel
  strToLogLevel s = case s of
    "quiet"  -> LogQuiet
    "normal" -> LogNormal
    _        -> LogDebug

handleSqlErr :: A.ToJSON a => Logger -> IO a -> IO a
handleSqlErr logg = handle $ checkSqlErr $ throw ErrBadRequest
 where
  checkSqlErr :: A.ToJSON a => IO a -> SqlError -> IO a
  checkSqlErr x e = printErr e >> x

  printErr :: SqlError -> IO ()
  printErr (SqlError q w t e r) = logg LogQuiet $ B8.unpack $ B8.intercalate
    " "
    [q, B8.pack (show w), e, r, t]

rIfJsonBody
  :: (FromJSON a, ToJSON b, Show b)
  => ResultResponseError
  -> MyHandler a b
  -> MyApp
rIfJsonBody rs x conn logg req respond = do
  j <- bodyToJSON req
  q <- try $ maybe (throw rs) (handleSqlErr (logg) . (x conn logg)) j
  respond $ eitherToResponse q
 where
  bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
  bodyToJSON j = A.decode <$> lazyRequestBody j

normalHandler :: (A.FromJSON a, A.ToJSON b, Show b) => MyHandler a b -> MyApp
normalHandler = rIfJsonBody ErrBadRequest

adminHandler :: (A.FromJSON a, A.ToJSON b, Show b) => MyHandler a b -> MyApp
adminHandler = rIfJsonBody ErrNotFound

eitherToResponse :: A.ToJSON b => Either ResultResponseError b -> Response
eitherToResponse r = case r of
  Right j -> responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j
  Left  ErrNotFound -> responseBuilder status404 [] "404 Not Found"
  Left  ErrInternal -> responseBuilder status500 [] "500 Internal Server Error"
  Left  p           -> e p
 where
  e :: ResultResponseError -> Response
  e = responseBuilder status400 jsonCT . toErr . show

  toErr :: String -> Builder
  toErr s = fromByteString $ B8.pack $ "{\"error\":\"" ++ s ++ "\"}"

  jsonCT :: [(HeaderName, B.ByteString)]
  jsonCT = [("Content-Type", "application/json")]

rIfDB :: ToRow a => Connection -> Query -> a -> ResultResponseError -> IO Bool
rIfDB c q val rElse = do
  p <- query c q val :: IO [Only Bool]
  case p of
    [Only True] -> return True
    _           -> throw rElse

isAdmin :: Connection -> String -> IO Bool
isAdmin conn token = rIfDB
  conn
  (Query "select admin from users where token=? limit 1;")
  [token]
  ErrNotFound

isAuthor :: Connection -> String -> IO Bool
isAuthor c token = rIfDB
  c
  "select count(id)=1 from authors where user_id=(select id from users where token=?);"
  [token]
  ErrNotAuthor

isUser :: Connection -> String -> IO Bool
isUser conn token = rIfDB
  conn
  (Query "select count(id)=1 from users where token=?;")
  [token]
  ErrNotUser

ifLoginExist :: Connection -> String -> IO Bool
ifLoginExist c login = ifLogin 1 c login ErrLoginNotExist

ifLoginNotExist :: Connection -> String -> IO Bool
ifLoginNotExist c login = ifLogin 0 c login ErrLoginAlreadyExist

ifLogin :: Int -> Connection -> String -> ResultResponseError -> IO Bool
ifLogin cond c login rElse =
  rIfDB c "select count(id)=? from users where login=?;" (cond, login) rElse

ifAuthorExist :: Connection -> String -> IO Bool
ifAuthorExist c login = rIfDB
  c
  "select count(id)=1 from authors where user_id=(select id from users where login=?);"
  [login]
  ErrAuthorNotExist

ifCategoryExist :: Connection -> Int -> IO Bool
ifCategoryExist c cat = rIfDB
  c
  "select count(id)=1 from categories where id=?;"
  [cat]
  ErrCategoryNotExist

ifTagNotExist :: Connection -> String -> IO Bool
ifTagNotExist c tag =
  rIfDB c "select count(id)=0 from tags where name=?;" [tag] ErrTagAlreadyExist

ifTagExist :: Connection -> Int -> IO Bool
ifTagExist c tag_id =
  rIfDB c "select count(id)=1 from tags where id=?;" [tag_id] ErrTagNotExist

ifNewsExist :: Connection -> Int -> IO Bool
ifNewsExist c news_id =
  rIfDB c "select count(id)=1 from news where id=?;" [news_id] ErrNewsNotExist

ifNewsPublished :: Connection -> Int -> IO Bool
ifNewsPublished c news_id = rIfDB
  c
  "select count(id)=1 from news where id=? and published=true;"
  [news_id]
  ErrNewsNotExist

ifNewsAuthor :: Connection -> Int -> String -> IO Bool
ifNewsAuthor c news_id token = rIfDB
  c
  "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
  (news_id, token)
  ErrNotYourNews

isValidPage :: Int -> IO Bool
isValidPage p = if p > 0 then return True else throw ErrBadPage

execResult :: GHC.Int.Int64 -> IO String
execResult i = case i of
  1 -> return ok
  _ -> throw ErrBadRequest

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

createImagesDir :: Logger -> IO ()
createImagesDir l = do
  handle (\e -> l LogQuiet (show (e :: IOException)) >> throw e)
    $ createDirectoryIfMissing False imagesDir
  p <- getPermissions imagesDir
  if readable p && writable p
    then return ()
    else do
      let e = imagesDir ++ " access denied"
      l LogQuiet e
      error e

deleteFile :: Logger -> FilePath -> IO ()
deleteFile l f =
  removeFile f
    `catch` (\e ->
              l LogQuiet ("Cannot delete file. " ++ (show (e :: IOException)))
                >> throw ErrInternal
            )

saveFile :: Logger -> FilePath -> B.ByteString -> IO ()
saveFile l f dat =
  B.writeFile f dat
    `catch` (\e ->
              l LogQuiet ("Cannot save file. " ++ (show (e :: IOException)))
                >> throw ErrInternal
            )
pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList = catMaybes . fromPGArray

calcOffset :: Int -> Int -> Int
calcOffset page perpage = (page - 1) * perpage

makeExt :: Maybe String -> String
makeExt = maybe ".jpg" ((++) "." . (map toLower))

decodeBase64 :: String -> B.ByteString
decodeBase64 = decodeLenient . fromString
