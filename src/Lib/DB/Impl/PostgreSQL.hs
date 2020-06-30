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
  { DB.createUser          = f сreateUser
  , DB.createUserWithPhoto = f сreateUserWithPhoto
  , DB.getUsers            = f getUsers
  , DB.deleteUser          = f deleteUser
  , DB.loginUser           = f loginUser
  , DB.deleteAuthor        = f deleteAuthor
  , DB.editAuthor          = f editAuthor
  , DB.makeAuthor          = f makeAuthor
  , DB.getAuthors          = f getAuthors
  , DB.createCategory      = f createCategory
  , DB.deleteCategory      = f deleteCategory
  , DB.editCategory        = f editCategory
  , DB.isLoginNotExist     = f isLoginNotExist
  , DB.isLoginExist        = f isLoginExist
  , DB.isAuthorExist       = f isAuthorExist
  , DB.isAdmin             = f isAdmin
  , DB.isAuthor            = f isAuthor
  , DB.isUser              = f isUser
  , DB.isCategoryExist     = f isCategoryExist
  , DB.isTagNotExist       = f isTagNotExist
  , DB.isTagExist          = f isTagExist
  , DB.isNewsExist         = f isNewsExist
  , DB.isNewsPublished     = f isNewsPublished
  , DB.thisNewsAuthor      = f thisNewsAuthor
  , DB.saveImage           = saveImage logger
  , DB.deleteFile          = deleteFile logger
  }
  where f x = x conn logger

execResult :: GHC.Int.Int64 -> DB.MaybeResult Bool
execResult i = case i of
  1 -> return $ Just True
  _ -> return Nothing

calcOffsetAndLimil :: Int -> Int -> [Int]
calcOffsetAndLimil page perPage =
  let offset = (page - 1) * perPage
      limit  = perPage
  in  [offset, limit]

catchErrorsMaybe :: Logger.Logger -> DB.MaybeResult a -> DB.MaybeResult a
catchErrorsMaybe logg func = do
  func
    `catch` (\(SqlError q w t e r) -> do
              logg Logger.LogQuiet $ B8.unpack $ B8.intercalate
                " "
                [q, B8.pack (show w), e, r, t]
              return Nothing
            )

catchErrorsEither :: Logger.Logger -> DB.EitherResult a -> DB.EitherResult a
catchErrorsEither logg func = do
  func
    `catch` (\(SqlError q w t e r) -> do
              logg Logger.LogQuiet $ B8.unpack $ B8.intercalate
                " "
                [q, B8.pack (show w), e, r, t]
              return $ Left ()
            )

decodeBase64 :: String -> B.ByteString
decodeBase64 = decodeLenient . fromString

rIfDB :: ToRow a => Connection -> Query -> a -> DB.Result Bool
rIfDB conn qry val = do
  p <- query conn qry val :: IO [Only Bool]
  case p of
    [Only a] -> return a
    _        -> return False

isLoginExist :: Connection -> Logger.Logger -> DB.Login -> DB.Result Bool
isLoginExist conn _ login = ifLogin 1 conn login

isLoginNotExist :: Connection -> Logger.Logger -> DB.Login -> DB.Result Bool
isLoginNotExist conn _ login = ifLogin 0 conn login

ifLogin :: Int -> Connection -> DB.Login -> DB.Result Bool
ifLogin cond conn login =
  rIfDB conn "select count(id)=? from users where login=?;" (cond, login)

isAdmin :: Connection -> Logger.Logger -> DB.Token -> DB.Result Bool
isAdmin conn _ token =
  rIfDB conn "select admin from users where token=? limit 1;" [token]

isAuthor :: Connection -> Logger.Logger -> DB.Token -> DB.Result Bool
isAuthor conn _ token = rIfDB
  conn
  "select count(id)=1 from authors where user_id=(select id from users where token=?);"
  [token]

isUser :: Connection -> Logger.Logger -> DB.Token -> IO Bool
isUser conn _ token =
  rIfDB conn (Query "select count(id)=1 from users where token=?;") [token]

isAuthorExist :: Connection -> Logger.Logger -> DB.Login -> DB.Result Bool
isAuthorExist conn _ login = rIfDB
  conn
  "select count(id)=1 from authors where user_id=(select id from users where login=?);"
  [login]

isCategoryExist
  :: Connection -> Logger.Logger -> DB.CategoryID -> DB.Result Bool
isCategoryExist conn _ catID =
  rIfDB conn "select count(id)=1 from categories where id=?;" [catID]

isTagNotExist :: Connection -> Logger.Logger -> DB.TagName -> DB.Result Bool
isTagNotExist conn _ tag =
  rIfDB conn "select count(id)=0 from tags where name=?;" [tag]

isTagExist :: Connection -> Logger.Logger -> DB.TagID -> DB.Result Bool
isTagExist conn _ tag_id =
  rIfDB conn "select count(id)=1 from tags where id=?;" [tag_id]

isNewsExist :: Connection -> Logger.Logger -> DB.NewsID -> DB.Result Bool
isNewsExist conn _ news_id =
  rIfDB conn "select count(id)=1 from news where id=?;" [news_id]

isNewsPublished :: Connection -> Logger.Logger -> DB.NewsID -> DB.Result Bool
isNewsPublished conn _ news_id = rIfDB
  conn
  "select count(id)=1 from news where id=? and published=true;"
  [news_id]

thisNewsAuthor
  :: Connection -> Logger.Logger -> DB.NewsID -> DB.Token -> DB.Result Bool
thisNewsAuthor conn _ news_id token = rIfDB
  conn
  "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
  (news_id, token)

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

сreateUser
  :: Connection
  -> Logger.Logger
  -> DB.Name
  -> DB.LastName
  -> DB.Login
  -> DB.Password
  -> DB.MaybeResult Bool
сreateUser conn logg name lastname login password =
  catchErrorsMaybe logg
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
сreateUserWithPhoto conn logg name lastname login password photoExt =
  catchErrorsMaybe logg $ do
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
getUsers conn logg page count = catchErrorsMaybe logg $ do
  r <- query conn
             "select name,lastname,photo from users offset ? limit ?;"
             (calcOffsetAndLimil page count)
  return $ Just r

deleteUser
  :: Connection -> Logger.Logger -> DB.Login -> DB.EitherResult DB.PhotoPath
deleteUser conn logg login = catchErrorsEither logg $ do
  q <-
    query conn "delete from users where login=? returning photo;" [login] :: IO
      [Maybe (Only String)]
  case q of
    [Just (Only f)] -> return $ Right $ Just f
    [Nothing      ] -> return $ Right Nothing
    _               -> return $ Left ()

loginUser
  :: Connection
  -> Logger.Logger
  -> DB.Login
  -> DB.Password
  -> DB.MaybeResult DB.Token
loginUser conn logg login password = catchErrorsMaybe logg $ do
  t <-
    query conn
          "select token from users where login=? and password=md5(?);"
          [login, password] :: IO [DB.Token]
  if null t then return Nothing else return $ Just (head t)

deleteAuthor :: Connection -> Logger.Logger -> DB.Login -> DB.MaybeResult Bool
deleteAuthor conn logg login =
  catchErrorsMaybe logg
    $   execute
          conn
          "delete from authors where user_id=(select id from users where login=?);"
          [login]
    >>= execResult

editAuthor
  :: Connection
  -> Logger.Logger
  -> DB.Login
  -> DB.Description
  -> DB.MaybeResult Bool
editAuthor conn logg login descr =
  catchErrorsMaybe logg
    $   execute
          conn
          "update authors set descr=? where user_id=(select id from users where login=?);"
          [descr, login]
    >>= execResult

getAuthors
  :: Connection
  -> Logger.Logger
  -> DB.Page
  -> DB.Count
  -> DB.MaybeResult [GetAuthors.Author]
getAuthors conn logg page count = catchErrorsMaybe logg $ do
  r <- query
    conn
    "select name,lastname,photo,descr from authors as a,users as u where a.user_id=u.id offset ? limit ?;"
    (calcOffsetAndLimil page count)
  return $ Just r

makeAuthor
  :: Connection
  -> Logger.Logger
  -> DB.Login
  -> DB.Description
  -> DB.MaybeResult Bool
makeAuthor conn logg login descr =
  catchErrorsMaybe logg
    $   execute
          conn
          "insert into authors (user_id,descr) values ((select id from users where login=?),?) on conflict (user_id) do update set descr=?;"
          [login, descr, descr]
    >>= execResult

createCategory
  :: Connection
  -> Logger.Logger
  -> DB.CategoryName
  -> DB.ParentCategory
  -> DB.MaybeResult Bool
createCategory conn logg name parent =
  catchErrorsMaybe logg
    $   execute
          conn
          "insert into categories (name,parent) values (?,(select id from categories where id=?)) on conflict do nothing;"
          (name, parent)
    >>= execResult

deleteCategory
  :: Connection -> Logger.Logger -> DB.CategoryID -> DB.MaybeResult Bool
deleteCategory conn logg catID =
  catchErrorsMaybe logg
    $   execute conn "delete from categories where id=?;" [catID]
    >>= execResult

editCategory
  :: Connection
  -> Logger.Logger
  -> DB.CategoryID
  -> DB.CategoryName
  -> DB.ParentCategory
  -> DB.MaybeResult Bool
editCategory conn logg catID name parent =
  catchErrorsMaybe logg
    $   execute conn
                "update categories set name=?, parent=? where id=?;"
                (name, parent, catID)
    >>= execResult
