{-# LANGUAGE OverloadedStrings #-}

module Lib.Functions
  ( returnFile
  , return404
  , handleSqlErr
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
  )
where

import           Lib.Constants
import           Lib.Types

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                , fromLazyByteString
                                                )
import           Control.Exception              ( handle )
import           Control.Monad                  ( when )
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
                                                , openFile
                                                )

import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )

readConfig :: IO Config
readConfig = do
  j   <- fromJust <$> A.decodeFileStrict "config.json" :: IO Conf
  c   <- connectPostgreSQL $ B8.pack $ pgconfig j
  hnd <- openFile "app.log" AppendMode
  return Config { connection = c, h = hnd, logger = lg }

lg :: Logger
lg = undefined

handleSqlErr :: A.ToJSON a => IO (ResultResponse a) -> IO (ResultResponse a)
handleSqlErr = handle $ checkSqlErr $ return ErrorBadRequest
 where
  checkSqlErr
    :: A.ToJSON a => IO (ResultResponse a) -> SqlError -> IO (ResultResponse a)
  checkSqlErr x e = printErr e >> x

  printErr :: SqlError -> IO ()
  printErr (SqlError q w t e r) =
    B8.putStrLn $ B8.intercalate " " [q, B8.pack (show w), e, r, t]

rIfJsonBody
  :: (FromJSON a, ToJSON b, Show b)
  => ResultResponse b
  -> MyHandler a b
  -> MyApp
rIfJsonBody rs x conn logg req respond = do
  j <- bodyToJSON req
  q <- maybe (return rs) (handleSqlErr . (x conn logg)) j
  respond $ resultToResponse q
 where
  bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
  bodyToJSON j = A.decode <$> lazyRequestBody j

normalHandler :: (A.FromJSON a, A.ToJSON b, Show b) => MyHandler a b -> MyApp
normalHandler = rIfJsonBody ErrorBadRequest

adminHandler :: (A.FromJSON a, A.ToJSON b, Show b) => MyHandler a b -> MyApp
adminHandler = rIfJsonBody Error404

resultToResponse :: (A.ToJSON a, Show a) => ResultResponse a -> Response
resultToResponse r = case r of
  Ok200 -> responseBuilder status200 jsonCT ok
  OkJSON j ->
    responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j
  Error404 -> responseBuilder status404 [] ""
  p        -> e p
 where
  ok :: Builder
  ok = fromByteString "{\"ok\":\"ok\"}"

  e :: Show a => ResultResponse a -> Response
  e = responseBuilder status400 jsonCT . toErr . show

  jsonCT :: [(HeaderName, B.ByteString)]
  jsonCT = [("Content-Type", "application/json")]

  toErr :: String -> Builder
  toErr s = fromByteString $ B8.pack $ "{\"error\":\"" ++ s ++ "\"}"

rIfDB
  :: ToRow a
  => Connection
  -> Query
  -> a
  -> IO (ResultResponse b)
  -> ResultResponse b
  -> IO (ResultResponse b)
rIfDB c q val r1 r2 = do
  p <- query c q val :: IO [Only Bool]
  case p of
    [Only True] -> r1
    _           -> return r2

rIfAdmin
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfAdmin conn token r = rIfDB
  conn
  (Query "select admin from users where token=? limit 1;")
  [token]
  r
  Error404

rIfAuthor
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfAuthor c token r = rIfDB
  c
  "select count(id)=1 from authors where user_id=(select id from users where token=?);"
  [token]
  r
  ErrorNotAuthor

rIfUser
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfUser conn token r = rIfDB
  conn
  (Query "select count(id)=1 from users where token=?;")
  [token]
  r
  ErrorNotUser

rIfLoginExist
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfLoginExist c login r = rIfLogin 1 c login r ErrorLoginNotExist

rIfLoginNotExist
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfLoginNotExist c login r = rIfLogin 0 c login r ErrorLoginAlreadyExist

rIfLogin
  :: Int
  -> Connection
  -> String
  -> IO (ResultResponse a)
  -> ResultResponse a
  -> IO (ResultResponse a)
rIfLogin cond c login r rElse =
  rIfDB c "select count(id)=? from users where login=?;" (cond, login) r rElse

rIfAuthorExist
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfAuthorExist c login r = rIfDB
  c
  "select count(id)=1 from authors where user_id=(select id from users where login=?);"
  [login]
  r
  ErrorAuthorNotExist

rIfCategoryExist
  :: Connection -> Int -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfCategoryExist c cat r = rIfDB
  c
  "select count(id)=1 from categories where id=?;"
  [cat]
  r
  ErrorCategoryNotExist

rIfTagNotExist
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfTagNotExist c tag r = rIfDB c
                               "select count(id)=0 from tags where name=?;"
                               [tag]
                               r
                               ErrorTagAlreadyExist

rIfTagExist
  :: Connection -> Int -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfTagExist c tag_id r =
  rIfDB c "select count(id)=1 from tags where id=?;" [tag_id] r ErrorTagNotExist

rIfNewsExist
  :: Connection -> Int -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfNewsExist c news_id r = rIfDB c
                                 "select count(id)=1 from news where id=?;"
                                 [news_id]
                                 r
                                 ErrorNewsNotExist

rIfNewsPublished
  :: Connection -> Int -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfNewsPublished c news_id r = rIfDB
  c
  "select count(id)=1 from news where id=? and published=true;"
  [news_id]
  r
  ErrorNewsNotExist

rIfNewsAuthor
  :: Connection
  -> Int
  -> String
  -> IO (ResultResponse a)
  -> IO (ResultResponse a)
rIfNewsAuthor c news_id token r = rIfDB
  c
  "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
  (news_id, token)
  r
  ErrorNotYourNews

rIfValidPage :: Int -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfValidPage p r = if p > 0 then r else return ErrorBadPage

rExecResult :: GHC.Int.Int64 -> IO (ResultResponse a)
rExecResult i = return $ case i of
  1 -> Ok200
  _ -> ErrorBadRequest

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
