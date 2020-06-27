{-# LANGUAGE OverloadedStrings #-}

module Lib.Logic.Impl.PostgreSQL
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
                                                ( ByteString )
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

import           Lib
import qualified Lib.Constants                 as Constants
import qualified Lib.FSUtils                   as FSUtils
import qualified Lib.Logger                    as Logger
import qualified Lib.Logic                     as Logic

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

type UserLogin = String

type UserToken = String

newHandle :: Connection -> Logger.Logger -> Logic.Handle
newHandle conn logger = Logic.Handle
  { Logic.getUsers     = catchE funcGetUsers
  , Logic.createUser   = catchE funcCreateUsers
  , Logic.deleteUser   = catchE funcDeleteUser
  , Logic.loginUser    = catchE funcLoginUser
  , Logic.deleteAuthor = catchE funcDeleteAuthor
  , Logic.editAuthor   = catchE funcEditAuthor
  }
  where catchE func req = catchErrors logger $ func conn logger req

execResult :: GHC.Int.Int64 -> Logic.MyResult b
execResult i = case i of
  1 -> return $ Logic.Success Nothing
  _ -> throw Logic.ErrorBadRequest

isValidPage :: Int -> IO Bool
isValidPage p = if p > 0 then return True else throw Logic.ErrorBadPage

calcOffsetAndLimil :: Int -> Int -> [Int]
calcOffsetAndLimil page perPage =
  let offset = (page - 1) * perPage
      limit  = perPage
  in  [offset, limit]

catchErrors :: Logger.Logger -> Logic.MyResult a -> Logic.MyResult a
catchErrors logg cmd = do
  cmd
    `catch` (\(SqlError q w t e r) -> do
              logg Logger.LogQuiet $ B8.unpack $ B8.intercalate
                " "
                [q, B8.pack (show w), e, r, t]
              return $ Logic.Error Logic.ErrorBadRequest
            )
    `catch` (\e -> return $ Logic.Error (e :: Logic.ResultResponseError))

makeExt :: Maybe String -> String
makeExt = maybe ".jpg" $ (++) "." . (map toLower)

decodeBase64 :: String -> B.ByteString
decodeBase64 = decodeLenient . fromString

rIfDB
  :: ToRow a => Connection -> Query -> a -> Logic.ResultResponseError -> IO Bool
rIfDB conn qry val rElse = do
  p <- query conn qry val :: IO [Only Bool]
  case p of
    [Only True] -> return True
    _           -> throw rElse

ifLoginExist :: Connection -> UserLogin -> IO Bool
ifLoginExist conn login = ifLogin 1 conn login Logic.ErrorLoginNotExist

ifLoginNotExist :: Connection -> UserLogin -> IO Bool
ifLoginNotExist conn login = ifLogin 0 conn login Logic.ErrorLoginAlreadyExist

ifLogin
  :: Int -> Connection -> UserLogin -> Logic.ResultResponseError -> IO Bool
ifLogin cond conn login rElse =
  rIfDB conn "select count(id)=? from users where login=?;" (cond, login) rElse

isAdmin :: Connection -> UserToken -> IO Bool
isAdmin conn token = rIfDB
  conn
  (Query "select admin from users where token=? limit 1;")
  [token]
  Logic.ErrorNotFound

isAuthor :: Connection -> UserToken -> IO Bool
isAuthor conn token = rIfDB
  conn
  "select count(id)=1 from authors where user_id=(select id from users where token=?);"
  [token]
  Logic.ErrorNotAuthor

isUser :: Connection -> UserToken -> IO Bool
isUser conn token = rIfDB
  conn
  (Query "select count(id)=1 from users where token=?;")
  [token]
  Logic.ErrorNotUser

ifAuthorExist :: Connection -> UserLogin -> IO Bool
ifAuthorExist conn login = rIfDB
  conn
  "select count(id)=1 from authors where user_id=(select id from users where login=?);"
  [login]
  Logic.ErrorAuthorNotExist

ifCategoryExist :: Connection -> Int -> IO Bool
ifCategoryExist conn cat = rIfDB
  conn
  "select count(id)=1 from categories where id=?;"
  [cat]
  Logic.ErrorCategoryNotExist

ifTagNotExist :: Connection -> String -> IO Bool
ifTagNotExist conn tag = rIfDB conn
                               "select count(id)=0 from tags where name=?;"
                               [tag]
                               Logic.ErrorTagAlreadyExist

ifTagExist :: Connection -> Int -> IO Bool
ifTagExist conn tag_id = rIfDB conn
                               "select count(id)=1 from tags where id=?;"
                               [tag_id]
                               Logic.ErrorTagNotExist

ifNewsExist :: Connection -> Int -> IO Bool
ifNewsExist conn news_id = rIfDB conn
                                 "select count(id)=1 from news where id=?;"
                                 [news_id]
                                 Logic.ErrorNewsNotExist

ifNewsPublished :: Connection -> Int -> IO Bool
ifNewsPublished conn news_id = rIfDB
  conn
  "select count(id)=1 from news where id=? and published=true;"
  [news_id]
  Logic.ErrorNewsNotExist

ifNewsAuthor :: Connection -> Int -> String -> IO Bool
ifNewsAuthor conn news_id token = rIfDB
  conn
  "select count(id)=1 from news where id=? and author_id=(select id from authors where user_id=(select id from users where token=?));"
  (news_id, token)
  Logic.ErrorNotYourNews

pgArrayToList :: PGArray (Maybe a) -> [a]
pgArrayToList = catMaybes . fromPGArray

funcCreateUsers
  :: Connection -> Logger.Logger -> Logic.MyHandler CreateUser.Request Bool
funcCreateUsers conn logg req =
  ifLoginNotExist conn (CreateUser.login req) >> case CreateUser.photo req of
    Nothing ->
      execute
          conn
          "insert into users (name,lastname,token,login,password) values(?,?,md5(random()::text),?,md5(?)) on conflict do nothing;"
          ( CreateUser.name req
          , CreateUser.lastname req
          , CreateUser.login req
          , CreateUser.password req
          )
        >>= execResult
    Just ph -> do
      let img = decodeBase64 ph
          ext = makeExt $ CreateUser.photo_type req
      q <- query
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
        [Only imgFile] -> do
          FSUtils.saveFile logg imgFile img
          return $ Logic.Success Nothing
        _ -> throw Logic.ErrorBadRequest

funcGetUsers
  :: Connection
  -> Logger.Logger
  -> Logic.MyHandler GetUsers.Request [GetUsers.User]
funcGetUsers conn logg req = isValidPage (GetUsers.page req) >> do
  r <- query conn
             "select name,lastname,photo from users offset ? limit ?;"
             (calcOffsetAndLimil (GetUsers.page req) usersPerPage)
  return $ Logic.Success $ Just r

funcDeleteUser
  :: Connection -> Logger.Logger -> Logic.MyHandler DeleteUser.Request Bool
funcDeleteUser conn logg req =
  isAdmin conn (DeleteUser.token req)
    >> ifLoginExist conn (DeleteUser.login req)
    >> do
         q <-
           query conn
                 "delete from users where login=? returning photo;"
                 [DeleteUser.login req] :: IO [Maybe (Only String)]
         case q of
           [Just (Only f)] ->
             logg Logger.LogDebug ("Removing file " ++ show (f))
               >> FSUtils.deleteFile logg f
               >> return (Logic.Success Nothing)
           [Nothing] -> return $ Logic.Success Nothing
           _         -> throw Logic.ErrorBadRequest

funcLoginUser
  :: Connection
  -> Logger.Logger
  -> Logic.MyHandler LoginUser.Request LoginUser.Token
funcLoginUser conn _ req = do
  t <-
    query conn
          "select token from users where login=? and password=md5(?);"
          [LoginUser.login req, LoginUser.password req] :: IO [LoginUser.Token]
  if null t
    then throw Logic.ErrorBadRequest
    else return $ Logic.Success $ Just (head t)

funcDeleteAuthor
  :: Connection -> Logger.Logger -> Logic.MyHandler DeleteAuthor.Request Bool
funcDeleteAuthor conn _ req =
  isAdmin conn (DeleteAuthor.token req)
    >>  ifAuthorExist conn (DeleteAuthor.login req)
    >>  execute
          conn
          "delete from authors where user_id=(select id from users where login=?);"
          [DeleteAuthor.login req]
    >>= execResult

funcEditAuthor
  :: Connection -> Logger.Logger -> Logic.MyHandler EditAuthor.Request Bool
funcEditAuthor conn _ u =
  isAdmin conn (EditAuthor.token u)
    >>  ifAuthorExist conn (EditAuthor.login u)
    >>  execute
          conn
          "update authors set descr=? where user_id=(select id from users where login=?);"
          [EditAuthor.descr u, EditAuthor.login u]
    >>= execResult
