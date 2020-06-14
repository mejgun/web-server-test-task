{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyApp
  , MyHandler
  , ResultResponse(..)
  , returnFile
  , return404
  , jsonCT
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
  , rExecResult
  , createImagesDir
  , pgArrayToList
  , calcOffset
  , normalHandler
  , adminHandler
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

import           PG

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

ok :: Builder
ok = fromByteString "{\"ok\":\"ok\"}"

jsonCT :: [(HeaderName, B.ByteString)]
jsonCT = [("Content-Type", "application/json")]

handleSqlErr :: A.ToJSON a => IO (ResultResponse a) -> IO (ResultResponse a)
handleSqlErr = handle $ checkSqlErr $ return ErrorBadRequest
 where
  checkSqlErr
    :: A.ToJSON a => IO (ResultResponse a) -> SqlError -> IO (ResultResponse a)
  checkSqlErr x e = printErr e >> x
  printErr :: SqlError -> IO ()
  printErr (SqlError q w t e r) =
    B8.putStrLn $ B8.intercalate " " [q, B8.pack (show w), e, r, t]

bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
bodyToJSON x = A.decode <$> lazyRequestBody x

rIfJsonBody
  :: (FromJSON a, ToJSON b) => ResultResponse b -> MyHandler a b -> MyApp
rIfJsonBody rs x conn req respond = do
  j <- bodyToJSON req
  q <- maybe (return rs) (x conn) j
  respond $ resultToResponse q

normalHandler :: (A.FromJSON a, A.ToJSON b) => MyHandler a b -> MyApp
normalHandler = rIfJsonBody ErrorBadRequest

adminHandler :: (A.FromJSON a, A.ToJSON b) => MyHandler a b -> MyApp
adminHandler = rIfJsonBody Error404

resultToResponse :: A.ToJSON a => ResultResponse a -> Response
resultToResponse r = case r of
  Ok200 -> responseBuilder status200 jsonCT ok
  OkJSON j ->
    responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j
  Error404          -> responseBuilder status404 [] ""
  ErrorBadRequest   -> e "bad request"
  ErrorNotAuthor    -> e "not a author"
  ErrorUserNotExist -> e "user not exist"
  where e x = responseBuilder status400 jsonCT $ toErr x

toErr :: String -> Builder
toErr s = fromByteString $ B8.pack $ "{\"error\":\"" ++ s ++ "\"}"

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
rIfUserExist c login r = rIfDB c
                               "select count(id)=1 from users where login=?;"
                               [login]
                               r
                               ErrorUserNotExist

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
