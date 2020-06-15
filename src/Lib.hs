{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( MyApp
  , MyHandler
  , ResultResponse(..)
  , returnFile
  , return404
  , handleSqlErr
  , usersPerPage
  , authorsPerPage
  , categoriesPerPage
  , tagsPerPage
  , commentsPerPage
  , newsPerPage
  , imagesDir
  , rIfAdmin
  , rIfAuthor
  , rIfUserExist
  , rIfUserNotExist
  , rIfAuthorExist
  , rExecResult
  , rIfValidPage
  , rIfCategoryExist
  , rIfTagNotExist
  , rIfTagExist
  , createImagesDir
  , pgArrayToList
  , calcOffset
  , normalHandler
  , adminHandler
  , module Lib.PG
  )
where

import           Blaze.ByteString.Builder       ( Builder
                                                , fromByteString
                                                , fromLazyByteString
                                                )
import           Control.Exception              ( handle )
import           Control.Monad                  ( when )
import           Data.Aeson                    as A
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
                                                ( intercalate
                                                , pack
                                                , putStrLn
                                                )
import           Data.Maybe                     ( catMaybes )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..)
                                                , Query(..)
                                                )
import qualified GHC.Int                        ( Int64 )
import           Lib.PG
import           Network.HTTP.Types             ( HeaderName
                                                , status200
                                                , status400
                                                , status404
                                                )
import           Network.Wai
import           System.Directory               ( createDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )

-- types

type MyApp
  =  Connection
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived

type MyHandler a b = Connection -> a -> IO (ResultResponse b)

data ResultResponse a = Ok200
    | OkJSON a
    | Error404
    | ErrorBadRequest
    | ErrorNotAuthor
    | ErrorUserNotExist
    | ErrorUserAlreadyExist
    | ErrorBadPage
    | ErrorAuthorNotExist
    | ErrorCategoryNotExist
    | ErrorTagAlreadyExist
    | ErrorTagNotExist
    deriving Show

-- constants

usersPerPage :: Int
usersPerPage = 10

authorsPerPage :: Int
authorsPerPage = 5

categoriesPerPage :: Int
categoriesPerPage = 15

tagsPerPage :: Int
tagsPerPage = 20

commentsPerPage :: Int
commentsPerPage = 10

newsPerPage :: Int
newsPerPage = 10

imagesDir :: String
imagesDir = "images/"

-- functions

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
rIfJsonBody rs x conn req respond = do
  j <- bodyToJSON req
  q <- maybe (return rs) (x conn) j
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
  (Query "select admin from users where token = ? limit 1;")
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

rIfUserExist
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfUserExist c login r = rIfUser 0 c login r ErrorUserNotExist

rIfUserNotExist
  :: Connection -> String -> IO (ResultResponse a) -> IO (ResultResponse a)
rIfUserNotExist c login r = rIfUser 0 c login r ErrorUserAlreadyExist

rIfUser
  :: Int
  -> Connection
  -> String
  -> IO (ResultResponse a)
  -> ResultResponse a
  -> IO (ResultResponse a)
rIfUser cond c login r rElse =
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
