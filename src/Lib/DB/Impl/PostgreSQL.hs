{-# LANGUAGE OverloadedStrings #-}

module Lib.DB.Impl.PostgreSQL
  ( newHandle
  )
where

import           Control.Exception              ( IOException
                                                , catch
                                                , handle
                                                , throw
                                                , try
                                                )
import qualified Data.ByteString               as B
                                                ( ByteString
                                                , writeFile
                                                )
import           Data.ByteString.Base64         ( decodeLenient )
import qualified Data.ByteString.Char8         as B8
import           Data.ByteString.UTF8           ( fromString )
import           Data.Maybe                     ( catMaybes )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types
                                                ( PGArray(..)
                                                , Query(..)
                                                )
import qualified GHC.Int                        ( Int64 )
import           System.Directory               ( removeFile )

import qualified Lib.Constants                 as Constants
import qualified Lib.DB                        as DB
import qualified Lib.Logger                    as Logger

import qualified Lib.Types.AddNewsComment      as AddNewsComment
import qualified Lib.Types.AddNewsPhoto        as AddNewsPhoto
import qualified Lib.Types.AddNewsTag          as AddNewsTag
import qualified Lib.Types.CreateCategory      as CreateCategory
import qualified Lib.Types.CreateNews          as CreateNews
import qualified Lib.Types.CreateTag           as CreateTag
import qualified Lib.Types.CreateUser          as CreateUser
import qualified Lib.Types.DeleteAuthor        as DeleteAuthor
import qualified Lib.Types.DeleteCategory      as DeleteCategory
import qualified Lib.Types.DeleteNews          as DeleteNews
import qualified Lib.Types.DeleteNewsComment   as DeleteNewsComment
import qualified Lib.Types.DeleteNewsPhoto     as DeleteNewsPhoto
import qualified Lib.Types.DeleteNewsTag       as DeleteNewsTag
import qualified Lib.Types.DeleteTag           as DeleteTag
import qualified Lib.Types.DeleteUser          as DeleteUser
import qualified Lib.Types.EditAuthor          as EditAuthor
import qualified Lib.Types.EditCategory        as EditCategory
import qualified Lib.Types.EditTag             as EditTag
import qualified Lib.Types.GetAuthors          as GetAuthors
import qualified Lib.Types.GetCategories       as GetCategories
import qualified Lib.Types.GetDrafts           as GetDrafts
import qualified Lib.Types.GetNews             as GetNews
import qualified Lib.Types.GetNewsComments     as GetNewsComments
import qualified Lib.Types.GetTags             as GetTags
import qualified Lib.Types.GetUsers            as GetUsers
import qualified Lib.Types.LoginUser           as LoginUser
import qualified Lib.Types.MakeAuthor          as MakeAuthor
import qualified Lib.Types.PublishNews         as PublishNews
import qualified Lib.Types.SetNewsMainPhoto    as SetNewsMainPhoto
import qualified Lib.Types.UpdateNews          as UpdateNews

newHandle :: Connection -> Logger.Logger -> DB.Handle
newHandle conn logger = DB.Handle
  { DB.createUser          = сreateUser conn logger
  , DB.createUserWithPhoto = сreateUserWithPhoto conn logger
  , DB.getUsers            = getUsers conn logger
  , DB.deleteUser          = deleteUser conn logger
  , DB.loginUser           = loginUser conn logger
  , DB.deleteAuthor        = deleteAuthor conn logger
  , DB.ifLoginNotExist     = ifLoginNotExist conn logger
  , DB.ifLoginExist        = ifLoginExist conn logger
  , DB.ifAuthorExist       = ifAuthorExist conn logger
  , DB.saveImage           = saveImage logger
  , DB.deleteFile          = deleteFile logger
  , DB.isAdmin             = isAdmin conn logger
  }

execResult :: GHC.Int.Int64 -> DB.MaybeResult Bool
execResult i = case i of
  1 -> return $ Just True
  _ -> return Nothing

calcOffsetAndLimil :: Int -> Int -> [Int]
calcOffsetAndLimil page perPage =
  let offset = (page - 1) * perPage
      limit  = perPage
  in  [offset, limit]

catchErrors :: Logger.Logger -> DB.MaybeResult a -> DB.MaybeResult a
catchErrors logg func = do
  func
    `catch` (\(SqlError q w t e r) -> do
              logg Logger.LogQuiet $ B8.unpack $ B8.intercalate
                " "
                [q, B8.pack (show w), e, r, t]
              return Nothing
            )

decodeBase64 :: String -> B.ByteString
decodeBase64 = decodeLenient . fromString

rIfDB :: ToRow a => Connection -> Query -> a -> DB.Result Bool
rIfDB conn qry val = do
  p <- query conn qry val :: IO [Only Bool]
  case p of
    [Only a] -> return a
    _        -> return False

ifLoginExist :: Connection -> Logger.Logger -> DB.Login -> DB.Result Bool
ifLoginExist conn _ login = ifLogin 1 conn login

ifLoginNotExist :: Connection -> Logger.Logger -> DB.Login -> DB.Result Bool
ifLoginNotExist conn _ login = ifLogin 0 conn login

ifLogin :: Int -> Connection -> DB.Login -> DB.Result Bool
ifLogin cond conn login =
  rIfDB conn "select count(id)=? from users where login=?;" (cond, login)

isAdmin :: Connection -> Logger.Logger -> DB.Token -> DB.Result Bool
isAdmin conn _ token =
  rIfDB conn "select admin from users where token=? limit 1;" [token]

-- isAuthor :: Connection -> UserToken -> IO Bool
-- isAuthor conn token = rIfDB
--   conn
--   "select count(id)=1 from authors where user_id=(select id from users where token=?);"
--   [token]
--   Logic.ErrorNotAuthor
-- isUser :: Connection -> UserToken -> IO Bool
-- isUser conn token = rIfDB
--   conn
--   (Query "select count(id)=1 from users where token=?;")
--   [token]
--   Logic.ErrorNotUser
ifAuthorExist :: Connection -> Logger.Logger -> DB.Login -> DB.Result Bool
ifAuthorExist conn _ login = rIfDB
  conn
  "select count(id)=1 from authors where user_id=(select id from users where login=?);"
  [login]
-- ifCategoryExist :: Connection -> Int -> IO Bool
-- ifCategoryExist conn cat = rIfDB
--   conn
--   "select count(id)=1 from categories where id=?;"
--   [cat]
--   Logic.ErrorCategoryNotExist
-- ifTagNotExist :: Connection -> String -> IO Bool
-- ifTagNotExist conn tag = rIfDB conn
--                                "select count(id)=0 from tags where name=?;"
--                                [tag]
--                                Logic.ErrorTagAlreadyExist
-- ifTagExist :: Connection -> Int -> IO Bool
-- ifTagExist conn tag_id = rIfDB conn
--                                "select count(id)=1 from tags where id=?;"
--                                [tag_id]
--                                Logic.ErrorTagNotExist
-- ifNewsExist :: Connection -> Int -> IO Bool
-- ifNewsExist conn news_id = rIfDB conn
--                                  "select count(id)=1 from news where id=?;"
--                                  [news_id]
--                                  Logic.ErrorNewsNotExist
-- ifNewsPublished :: Connection -> Int -> IO Bool
-- ifNewsPublished conn news_id = rIfDB
--   conn
--   "select count(id)=1 from news where id=? and published=true;"
--   [news_id]
--   Logic.ErrorNewsNotExist
-- ifNewsAuthor :: Connection -> Int -> String -> IO Bool
-- ifNewsAuthor conn news_id token = rIfDB
--   conn
--   "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
--   (news_id, token)
--   Logic.ErrorNotYourNews
-- pgArrayToList :: PGArray (Maybe a) -> [a]
-- pgArrayToList = catMaybes . fromPGArray
сreateUser
  :: Connection
  -> Logger.Logger
  -> DB.Name
  -> DB.LastName
  -> DB.Login
  -> DB.Password
  -> DB.MaybeResult Bool
сreateUser conn logg name lastname login password =
  catchErrors logg
    $   execute
          conn
          "insert into users (name,lastname,token,login,password) values(?,?,md5(random()::text),?,md5(?)) on conflict do nothing;"
          (name, lastname, login, password)
    >>= execResult

сreateUserWithPhoto
  :: Connection
  -> Logger.Logger
  -> DB.Name
  -> DB.LastName
  -> DB.Login
  -> DB.Password
  -> DB.PhotoExt
  -> DB.MaybeResult DB.PhotoPath
сreateUserWithPhoto conn logg name lastname login password photoExt = do
  q <- query
    conn
    "insert into users (name,lastname,token,login,password,photo) values(?,?,md5(random()::text),?,md5(?),concat(?,md5(random()::text),?)) on conflict do nothing returning photo;"
    (name, lastname, login, password, Constants.imagesDir, photoExt)
  case q of
    [Only imgFile] -> return $ Just imgFile
    _              -> return Nothing

getUsers
  :: Connection
  -> Logger.Logger
  -> DB.Page
  -> DB.Count
  -> DB.MaybeResult [GetUsers.User]
getUsers conn logg page count = catchErrors logg $ do
  r <- query conn
             "select name,lastname,photo from users offset ? limit ?;"
             (calcOffsetAndLimil page count)
  return $ Just r

deleteUser
  :: Connection -> Logger.Logger -> DB.Login -> DB.EitherResult DB.PhotoPath
deleteUser conn _ login = do
  q <-
    query conn "delete from users where login=? returning photo;" [login] :: IO
      [Maybe (Only String)]
  case q of
    [Just (Only f)] -> return $ Right $ Just f
    [Nothing      ] -> return $ Right Nothing
    _               -> return $ Left False

loginUser
  :: Connection
  -> Logger.Logger
  -> DB.Login
  -> DB.Password
  -> DB.MaybeResult DB.Token
loginUser conn _ login password = do
  t <-
    query conn
          "select token from users where login=? and password=md5(?);"
          [login, password] :: IO [DB.Token]
  if null t then return Nothing else return $ Just (head t)

deleteAuthor :: Connection -> Logger.Logger -> DB.Login -> DB.MaybeResult Bool
deleteAuthor conn _ login =
  execute
      conn
      "delete from authors where user_id=(select id from users where login=?);"
      [login]
    >>= execResult

-- funcEditAuthor
--   :: Connection -> Logger.Logger -> Logic.MyHandler EditAuthor.Request Bool
-- funcEditAuthor conn _ u =
--   isAdmin conn (EditAuthor.token u)
--     >>  ifAuthorExist conn (EditAuthor.login u)
--     >>  execute
--           conn
--           "update authors set descr=? where user_id=(select id from users where login=?);"
--           [EditAuthor.descr u, EditAuthor.login u]
--     >>= execResult

deleteFile :: Logger.Logger -> FilePath -> DB.MaybeResult Bool
deleteFile logg file = do
  res <- try $ removeFile file
  case res of
    Left e -> do
      logg Logger.LogQuiet $ "Cannot delete file. " <> show (e :: IOException)
      return Nothing
    _ -> return $ Just True

saveImage :: Logger.Logger -> FilePath -> String -> DB.MaybeResult Bool
saveImage logg file str = do
  let dat = decodeBase64 str
  res <- try $ B.writeFile file dat
  case res of
    Left e -> do
      logg Logger.LogQuiet $ "Cannot save file. " <> show (e :: IOException)
      return Nothing
    _ -> return $ Just True
