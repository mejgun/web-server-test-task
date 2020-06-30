{-# LANGUAGE OverloadedStrings #-}

module Lib.Handlers
  ( ResultResponseError(..)
  , Result
  , getUsers
  , createUser
  , deleteUser
  , loginUser
  , deleteAuthor
  , editAuthor
  , getAuthors
  , makeAuthor
  , createCategory
  , deleteCategory
  , editCategory
  , getCategories
  , createTag
  , deleteTag
  , editTag
  , getTags
  , addNewsComment
  , addNewsPhoto
  , addNewsTag
  , createNews
  , deleteNews
  , deleteNewsComment
  , deleteNewsPhoto
  , deleteNewsTag
  , getDrafts
  , getNews
  , getNewsComments
  , publishNews
  , setNewsMainPhoto
  , updateNews
  )
where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )

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

import qualified Lib.Constants                 as Constants
import qualified Lib.DB                        as DB
import qualified Lib.Logger                    as Logger

data ResultResponseError
  = ErrorNotFound
  | ErrorBadRequest
  | ErrorNotAuthor
  | ErrorLoginNotExist
  | ErrorLoginAlreadyExist
  | ErrorBadPage
  | ErrorCategoryNotExist
  | ErrorTagAlreadyExist
  | ErrorTagNotExist
  | ErrorNewsNotExist
  | ErrorNotYourNews
  | ErrorNotUser
  | ErrorAuthorNotExist
  | ErrorInternal
  deriving (Show)

instance Exception ResultResponseError

type Result = IO

createUser :: DB.Handle -> CreateUser.Request -> Result String
createUser dbH req = do
  exist <- DB.isLoginNotExist dbH (CreateUser.login req)
  unless exist (throw ErrorLoginAlreadyExist)
  if isJust (CreateUser.photo req)
    then do
      let ext = makeExt (CreateUser.photo_type req)
      r <- DB.createUserWithPhoto dbH
                                  (CreateUser.name req)
                                  (CreateUser.lastname req)
                                  (CreateUser.login req)
                                  (CreateUser.password req)
                                  ext
      case r of
        Just fileName -> do
          DB.saveImage dbH fileName $ fromJust (CreateUser.photo req)
          return justOK
        _ -> throw ErrorBadRequest
    else do
      r <- DB.createUser dbH
                         (CreateUser.name req)
                         (CreateUser.lastname req)
                         (CreateUser.login req)
                         (CreateUser.password req)
      if isJust r then return justOK else throw ErrorBadRequest

getUsers :: DB.Handle -> GetUsers.Request -> Result [GetUsers.User]
getUsers dbH req = do
  unless (isValidPage (GetUsers.page req)) (throw ErrorBadPage)
  r <- DB.getUsers dbH (GetUsers.page req) Constants.usersPerPage
  case r of
    Just users -> return users
    _          -> throw ErrorBadRequest

deleteUser :: DB.Handle -> DeleteUser.Request -> Result String
deleteUser dbH req = do
  admin <- DB.isAdmin dbH (DeleteUser.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isLoginExist dbH (DeleteUser.login req)
  unless exist (throw ErrorBadRequest)
  photo <- DB.deleteUser dbH (DeleteUser.login req)
  case photo of
    Right (Just f) -> DB.deleteFile dbH f >> return justOK
    Left  _        -> throw ErrorBadRequest
    _              -> return justOK

loginUser :: DB.Handle -> LoginUser.Request -> Result LoginUser.Token
loginUser dbH req = do
  res <- DB.loginUser dbH (LoginUser.login req) (LoginUser.password req)
  case res of
    Just token -> return $ LoginUser.Token { LoginUser.token = token }
    _          -> throw ErrorBadRequest

deleteAuthor :: DB.Handle -> DeleteAuthor.Request -> Result String
deleteAuthor dbH req = do
  admin <- DB.isAdmin dbH (DeleteAuthor.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isAuthorExist dbH (DeleteAuthor.login req)
  unless exist (throw ErrorAuthorNotExist)
  res <- DB.deleteAuthor dbH (DeleteAuthor.login req)
  if isJust res then return justOK else throw ErrorBadRequest

editAuthor :: DB.Handle -> EditAuthor.Request -> Result String
editAuthor dbH req = do
  admin <- DB.isAdmin dbH (EditAuthor.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isAuthorExist dbH (EditAuthor.login req)
  unless exist (throw ErrorAuthorNotExist)
  res <- DB.editAuthor dbH (EditAuthor.login req) (EditAuthor.descr req)
  if isJust res then return justOK else throw ErrorBadRequest

getAuthors :: DB.Handle -> GetAuthors.Request -> Result [GetAuthors.Author]
getAuthors dbH req = do
  admin <- DB.isAdmin dbH (GetAuthors.token req)
  unless admin                               (throw ErrorNotFound)
  unless (isValidPage (GetAuthors.page req)) (throw ErrorBadPage)
  res <- DB.getAuthors dbH (GetAuthors.page req) Constants.authorsPerPage
  case res of
    Just authors -> return authors
    _            -> throw ErrorBadRequest

makeAuthor :: DB.Handle -> MakeAuthor.Request -> Result String
makeAuthor dbH req = do
  admin <- DB.isAdmin dbH (MakeAuthor.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isLoginExist dbH (MakeAuthor.login req)
  unless exist (throw ErrorLoginNotExist)
  res <- DB.makeAuthor dbH (MakeAuthor.login req) (MakeAuthor.descr req)
  if isJust res then return justOK else throw ErrorBadRequest

createCategory :: DB.Handle -> CreateCategory.Request -> Result String
createCategory dbH req = do
  admin <- DB.isAdmin dbH (CreateCategory.token req)
  unless admin (throw ErrorNotFound)
  res <- DB.createCategory dbH
                           (CreateCategory.name req)
                           (CreateCategory.parent req)
  if isJust res then return justOK else throw ErrorBadRequest

deleteCategory :: DB.Handle -> DeleteCategory.Request -> Result String
deleteCategory dbH req = do
  admin <- DB.isAdmin dbH (DeleteCategory.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isCategoryExist dbH (DeleteCategory.cat_id req)
  unless exist (throw ErrorCategoryNotExist)
  res <- DB.deleteCategory dbH (DeleteCategory.cat_id req)
  if isJust res then return justOK else throw ErrorBadRequest

editCategory :: DB.Handle -> EditCategory.Request -> Result String
editCategory dbH req = undefined

getCategories
  :: DB.Handle -> GetCategories.Request -> Result [GetCategories.Cat]
getCategories dbH req = undefined

createTag :: DB.Handle -> CreateTag.Request -> Result String
createTag dbH req = undefined

deleteTag :: DB.Handle -> DeleteTag.Request -> Result String
deleteTag dbH req = undefined

editTag :: DB.Handle -> EditTag.Request -> Result String
editTag dbH req = undefined

getTags :: DB.Handle -> GetTags.Request -> Result [GetTags.Tag]
getTags dbH req = undefined

addNewsComment :: DB.Handle -> AddNewsComment.Request -> Result String
addNewsComment dbH req = undefined

addNewsPhoto :: DB.Handle -> AddNewsPhoto.Request -> Result String
addNewsPhoto dbH req = undefined

addNewsTag :: DB.Handle -> AddNewsTag.Request -> Result String
addNewsTag dbH req = undefined

createNews :: DB.Handle -> CreateNews.Request -> Result CreateNews.NewsId
createNews dbH req = undefined

deleteNews :: DB.Handle -> DeleteNews.Request -> Result String
deleteNews dbH req = undefined

deleteNewsComment :: DB.Handle -> DeleteNewsComment.Request -> Result String
deleteNewsComment dbH req = undefined

deleteNewsPhoto :: DB.Handle -> DeleteNewsPhoto.Request -> Result String
deleteNewsPhoto dbH req = undefined

deleteNewsTag :: DB.Handle -> DeleteNewsTag.Request -> Result String
deleteNewsTag dbH req = undefined

getDrafts :: DB.Handle -> GetDrafts.Request -> Result [GetDrafts.Draft]
getDrafts dbH req = undefined

getNews :: DB.Handle -> GetNews.Request -> Result [GetNews.News]
getNews dbH req = undefined

getNewsComments
  :: DB.Handle -> GetNewsComments.Request -> Result [GetNewsComments.Comment]
getNewsComments dbH req = undefined

publishNews :: DB.Handle -> PublishNews.Request -> Result String
publishNews dbH req = undefined

setNewsMainPhoto :: DB.Handle -> SetNewsMainPhoto.Request -> Result String
setNewsMainPhoto dbH req = undefined

updateNews :: DB.Handle -> UpdateNews.Request -> Result String
updateNews dbH req = undefined

isValidPage :: Int -> Bool
isValidPage = (> 0)

justOK :: String
justOK = "ok"

makeExt :: Maybe String -> String
makeExt = maybe ".jpg" $ (++) "." . (map toLower)

ifAdmin :: DB.Handle -> DB.Login -> DB.Result ()
ifAdmin dbH token = do
  admin <- DB.isAdmin dbH token
  unless admin (throw ErrorNotFound)
