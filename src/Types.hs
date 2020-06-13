{-# LANGUAGE OverloadedStrings #-}

module Types
  ( MyApp
  , MyHandler
  , responseOK
  , responseERR
  , responseSQLERR
  , respJSON
  , returnFile
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
                                                ( PGArray(..) )
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

type MyHandler a = Connection -> a -> IO Response

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

isAdmin :: Connection -> String -> IO Bool
isAdmin conn token = do
  p <- query conn "select admin from users where token = ? limit 1;" [token]
  return $ case p of
    [Only i] -> i
    _        -> False

isAuthor :: Connection -> String -> IO Bool
isAuthor conn token = do
  p <- query
    conn
    "select count(id)=1 from authors where user_id=(select id from users where token=?);"
    [token]
  return $ case p of
    [Only i] -> i
    _        -> False

ok :: Builder
ok = fromByteString "{\"ok\":\"ok\"}"

responseOK :: Response
responseOK = responseBuilder status200 jsonCT ok

err :: Builder
err = fromByteString "{\"error\":\"bad request\"}"

responseERR :: Response
responseERR = responseBuilder status404 [] ""

responseSQLERR :: Response
responseSQLERR = responseBuilder status400 jsonCT err

respJSON :: (A.ToJSON a) => a -> Response
respJSON j = responseBuilder status200 jsonCT $ fromLazyByteString $ A.encode j

jsonCT :: [(HeaderName, B.ByteString)]
jsonCT = [("Content-Type", "application/json")]

handleSqlErr :: IO Response -> IO Response
handleSqlErr = handle $ checkSqlErr $ return responseSQLERR
 where
  checkSqlErr :: IO Response -> SqlError -> IO Response
  checkSqlErr x e = printErr e >> x
  printErr :: SqlError -> IO ()
  printErr (SqlError q w t e r) =
    B8.putStrLn $ B8.intercalate " " [q, B8.pack (show w), e, r, t]

bodyToJSON :: A.FromJSON a => Request -> IO (Maybe a)
bodyToJSON x = A.decode <$> lazyRequestBody x

rIfJsonBody :: A.FromJSON a => Response -> MyHandler a -> MyApp
rIfJsonBody rs x conn req respond = do
  j <- bodyToJSON req
  q <- maybe (return rs) (x conn) j
  respond q

normalHandler :: A.FromJSON a => MyHandler a -> MyApp
normalHandler = rIfJsonBody responseSQLERR

adminHandler :: A.FromJSON a => MyHandler a -> MyApp
adminHandler = rIfJsonBody responseERR

rIfAdmin :: Connection -> String -> IO Response -> IO Response
rIfAdmin c t r = responseIf isAdmin c t r responseERR

rIfAuthor :: Connection -> String -> IO Response -> IO Response
rIfAuthor c t r = responseIf isAuthor c t r responseSQLERR

rExecResult :: GHC.Int.Int64 -> IO Response
rExecResult i = return $ case i of
  1 -> responseOK
  _ -> responseSQLERR

responseIf
  :: (Connection -> String -> IO Bool)
  -> Connection
  -> String
  -> IO Response
  -> Response
  -> IO Response
responseIf cond conn token r rElse = do
  a <- cond conn token
  if a then r else return rElse

returnFile :: T.Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
returnFile f rd = do
  let file = imagesDir ++ (T.unpack f)
  exist <- doesFileExist file
  rd $ case exist of
    True -> do
      responseFile status200 contentType file Nothing
    _ -> responseERR
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
