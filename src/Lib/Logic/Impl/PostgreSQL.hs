{-# LANGUAGE OverloadedStrings #-}

module Lib.Logic.Impl.PostgreSQL
  ( newHandle
  )
where

import           Control.Exception              ( IOException
                                                , catch
                                                , handle
                                                , try
                                                )
import           Control.Monad.Except
import qualified Data.ByteString               as B
                                                ( ByteString
                                                , writeFile
                                                )
import           Data.ByteString.Base64         ( decodeLenient )
import qualified Data.ByteString.Char8         as B8
import           Data.ByteString.UTF8           ( fromString )
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( catMaybes )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..)
                                                , Query(..)
                                                )
import qualified GHC.Int                        ( Int64 )
import           System.Directory               ( removeFile )

import           Lib                     hiding ( ifLoginNotExist
                                                , isValidPage
                                                )
import qualified Lib.Constants                 as Constants
import qualified Lib.FSUtils                   as FSUtils
import qualified Lib.Logger                    as Logger
import qualified Lib.Logic                     as Logic
import qualified Lib.Requests.CreateUser       as CreateUser
import qualified Lib.Requests.GetUsers         as GetUsers

type Login = String

newHandle :: Connection -> Logger.Logger -> Logic.Handle
newHandle conn logger = Logic.Handle
  { Logic.getUsers   = funcGetUsers conn logger
  , Logic.createUser = funcCreateUsers conn logger
  }

executeResult :: GHC.Int.Int64 -> Logic.ExceptMonad String
executeResult i = case i of
  1 -> return ok
  _ -> throwError Logic.ErrorBadRequest

isValidPage :: Int -> Logic.ExceptMonad Bool
isValidPage p = if p > 0 then return True else throwError Logic.ErrorBadPage

calcOffsetAndLimil :: Int -> Int -> [Int]
calcOffsetAndLimil page perPage =
  let offset = (page - 1) * perPage
      limit  = perPage
  in  [offset, limit]

rIfDB
  :: ToRow a
  => Connection
  -> Query
  -> a
  -> Logic.ResultResponseError
  -> Logic.ExceptMonad Bool
rIfDB conn qry val rElse = do
  p <- liftIO (query conn qry val :: IO [Only Bool])
  case p of
    [Only True] -> return True
    _           -> throwError rElse

ifLoginExist :: Connection -> Login -> Logic.ExceptMonad Bool
ifLoginExist conn login = ifLogin 1 conn login Logic.ErrorLoginNotExist

ifLoginNotExist :: Connection -> Login -> Logic.ExceptMonad Bool
ifLoginNotExist conn login = ifLogin 0 conn login Logic.ErrorLoginAlreadyExist

ifLogin
  :: Int
  -> Connection
  -> Login
  -> Logic.ResultResponseError
  -> Logic.ExceptMonad Bool
ifLogin cond conn login rElse =
  rIfDB conn "select count(id)=? from users where login=?;" (cond, login) rElse

-- isAdmin :: Connection -> String -> IO Bool
-- isAdmin conn token = rIfDB
--   conn
--   (Query "select admin from users where token=? limit 1;")
--   [token]
--   ErrorNotFound
-- isAuthor :: Connection -> String -> IO Bool
-- isAuthor c token = rIfDB
--   c
--   "select count(id)=1 from authors where user_id=(select id from users where token=?);"
--   [token]
--   ErrorNotAuthor
-- isUser :: Connection -> String -> IO Bool
-- isUser conn token = rIfDB
--   conn
--   (Query "select count(id)=1 from users where token=?;")
--   [token]
--   ErrorNotUser
-- ifAuthorExist :: Connection -> String -> IO Bool
-- ifAuthorExist c login = rIfDB
--   c
--   "select count(id)=1 from authors where user_id=(select id from users where login=?);"
--   [login]
--   ErrorAuthorNotExist
-- ifCategoryExist :: Connection -> Int -> IO Bool
-- ifCategoryExist c cat = rIfDB
--   c
--   "select count(id)=1 from categories where id=?;"
--   [cat]
--   ErrorCategoryNotExist
-- ifTagNotExist :: Connection -> String -> IO Bool
-- ifTagNotExist c tag = rIfDB c
--                             "select count(id)=0 from tags where name=?;"
--                             [tag]
--                             ErrorTagAlreadyExist
-- ifTagExist :: Connection -> Int -> IO Bool
-- ifTagExist c tag_id =
--   rIfDB c "select count(id)=1 from tags where id=?;" [tag_id] ErrorTagNotExist
-- ifNewsExist :: Connection -> Int -> IO Bool
-- ifNewsExist c news_id =
--   rIfDB c "select count(id)=1 from news where id=?;" [news_id] ErrorNewsNotExist
-- ifNewsPublished :: Connection -> Int -> IO Bool
-- ifNewsPublished c news_id = rIfDB
--   c
--   "select count(id)=1 from news where id=? and published=true;"
--   [news_id]
--   ErrorNewsNotExist
-- ifNewsAuthor :: Connection -> Int -> String -> IO Bool
-- ifNewsAuthor c news_id token = rIfDB
--   c
--   "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
--   (news_id, token)
--   ErrorNotYourNews

pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList = catMaybes . fromPGArray

makeExt :: Maybe String -> String
makeExt = maybe ".jpg" $ (++) "." . (map toLower)

decodeBase64 :: String -> B.ByteString
decodeBase64 = decodeLenient . fromString

safeExecute
  :: ToRow q
  => Logger.Logger
  -> Connection
  -> Query
  -> q
  -> Logic.ExceptMonad GHC.Int.Int64
safeExecute logg conn qry val = handleSqlErr logg $ execute conn qry val

safeQuery
  :: (ToRow q, FromRow b)
  => Logger.Logger
  -> Connection
  -> Query
  -> q
  -> Logic.ExceptMonad [b]
safeQuery logg conn qry val = handleSqlErr logg $ query conn qry val

funcCreateUsers
  :: Connection
  -> Logger.Logger
  -> CreateUser.Request
  -> Logic.ExceptMonad String
funcCreateUsers conn logg req =
  ifLoginNotExist conn (CreateUser.login req) >> case CreateUser.photo req of
    Nothing ->
      safeExecute
          logg
          conn
          "insert into users (name,lastname,token,login,password) values(?,?,md5(random()::text),?,md5(?)) on conflict do nothing;"
          ( CreateUser.name req
          , CreateUser.lastname req
          , CreateUser.login req
          , CreateUser.password req
          )
        >>= executeResult
    Just ph -> do
      let img = decodeBase64 ph
          ext = makeExt $ CreateUser.photo_type req
      q <- safeQuery
        logg
        conn
        "insert into users (name,lastname,token,login,password,photo) values(?,?,md5(random()::text),?,md5(?),concat(?,md5(random()::text),?)) on conflict do nothing returning photo;"
        ( CreateUser.name req
        , CreateUser.lastname req
        , CreateUser.login req
        , CreateUser.password req
        , Constants.imagesDir
        , ext
        )
      case q of
        [Only imgFile] -> saveFile logg imgFile img >> return ok
        _              -> throwError Logic.ErrorBadRequest

funcGetUsers
  :: Connection
  -> Logger.Logger
  -> GetUsers.Request
  -> Logic.ExceptMonad [GetUsers.User]
funcGetUsers conn logg req = isValidPage (GetUsers.page req) >> do
  r <- safeQuery logg
                 conn
                 "select name,lastname,photo from users offset ? limit ?;"
                 (calcOffsetAndLimil (GetUsers.page req) usersPerPage)
  return r

deleteFile :: Logger.Logger -> FilePath -> Logic.ExceptMonad Bool
deleteFile logg file = do
  res <- liftIO $ try $ removeFile file
  case res of
    Left e -> do
      liftIO $ logg Logger.LogQuiet $ "Cannot delete file. " <> show
        (e :: IOException)
      throwError Logic.ErrorInternal
    _ -> return True

saveFile :: Logger.Logger -> FilePath -> B.ByteString -> Logic.ExceptMonad Bool
saveFile logg file dat = do
  res <- liftIO $ try $ B.writeFile file dat
  case res of
    Left e -> do
      liftIO $ logg Logger.LogQuiet $ "Cannot save file. " <> show
        (e :: IOException)
      throwError Logic.ErrorInternal
    _ -> return True

handleSqlErr :: Logger.Logger -> IO a -> Logic.ExceptMonad a
handleSqlErr logg cmd = do
  res <- liftIO $ try cmd
  case res of
    Left (SqlError q w t e r) -> do
      liftIO $ logg Logger.LogQuiet $ B8.unpack $ B8.intercalate
        " "
        [q, B8.pack (show w), e, r, t]
      throwError Logic.ErrorBadRequest
    Right r -> return r
