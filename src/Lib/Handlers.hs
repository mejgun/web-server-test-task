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
  notexist <- DB.isLoginNotExist dbH (CreateUser.login req)
  unless notexist (throw ErrorLoginAlreadyExist)
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
editCategory dbH req = do
  admin <- DB.isAdmin dbH (EditCategory.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isCategoryExist dbH (EditCategory.cat_id req)
  unless exist (throw ErrorCategoryNotExist)
  res <- DB.editCategory dbH
                         (EditCategory.cat_id req)
                         (EditCategory.name req)
                         (EditCategory.parent req)
  if isJust res then return justOK else throw ErrorBadRequest

getCategories
  :: DB.Handle -> GetCategories.Request -> Result [GetCategories.Cat]
getCategories dbH req = do
  unless (isValidPage (GetCategories.page req)) (throw ErrorBadPage)
  res <- DB.getCategories dbH
                          (GetCategories.page req)
                          Constants.categoriesPerPage
  case res of
    Just categories -> return categories
    _               -> throw ErrorBadRequest

createTag :: DB.Handle -> CreateTag.Request -> Result String
createTag dbH req = do
  admin <- DB.isAdmin dbH (CreateTag.token req)
  unless admin (throw ErrorNotFound)
  notexist <- DB.isTagNotExist dbH (CreateTag.name req)
  unless notexist (throw ErrorTagAlreadyExist)
  res <- DB.createTag dbH (CreateTag.name req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

deleteTag :: DB.Handle -> DeleteTag.Request -> Result String
deleteTag dbH req = do
  admin <- DB.isAdmin dbH (DeleteTag.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isTagExist dbH (DeleteTag.tag_id req)
  unless exist (throw ErrorTagNotExist)
  res <- DB.deleteTag dbH (DeleteTag.tag_id req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

editTag :: DB.Handle -> EditTag.Request -> Result String
editTag dbH req = do
  admin <- DB.isAdmin dbH (EditTag.token req)
  unless admin (throw ErrorNotFound)
  exist <- DB.isTagExist dbH (EditTag.tag_id req)
  unless exist (throw ErrorTagNotExist)
  res <- DB.editTag dbH (EditTag.tag_id req) (EditTag.name req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

getTags :: DB.Handle -> GetTags.Request -> Result [GetTags.Tag]
getTags dbH req = do
  unless (isValidPage (GetTags.page req)) (throw ErrorBadPage)
  res <- DB.getTags dbH (GetTags.page req) Constants.tagsPerPage
  case res of
    Just tags -> return tags
    _         -> throw ErrorBadRequest

addNewsComment :: DB.Handle -> AddNewsComment.Request -> Result String
addNewsComment dbH req = do
  published <- DB.isNewsPublished dbH (AddNewsComment.news_id req)
  unless published (throw ErrorNewsNotExist)
  user <- DB.isUser dbH (AddNewsComment.token req)
  unless user (throw ErrorNotUser)
  res <- DB.addNewsComment dbH
                           (AddNewsComment.news_id req)
                           (AddNewsComment.text req)
                           (AddNewsComment.token req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

addNewsPhoto :: DB.Handle -> AddNewsPhoto.Request -> Result String
addNewsPhoto dbH req = do
  author <- DB.isAuthor dbH (AddNewsPhoto.token req)
  unless author (throw ErrorNotAuthor)
  exist <- DB.isNewsExist dbH (AddNewsPhoto.news_id req)
  unless exist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (AddNewsPhoto.news_id req)
                                  (AddNewsPhoto.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  let ext = makeExt (AddNewsPhoto.photo_type req)
  res <- DB.addNewsPhoto dbH
                         (AddNewsPhoto.news_id req)
                         (AddNewsPhoto.token req)
                         ext
  case res of
    Just imgFile -> do
      DB.saveImage dbH imgFile (AddNewsPhoto.photo req)
      return justOK
    _ -> throw ErrorBadRequest

addNewsTag :: DB.Handle -> AddNewsTag.Request -> Result String
addNewsTag dbH req = do
  author <- DB.isAuthor dbH (AddNewsTag.token req)
  unless author (throw ErrorNotAuthor)
  newsexist <- DB.isNewsExist dbH (AddNewsTag.news_id req)
  unless newsexist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (AddNewsTag.news_id req)
                                  (AddNewsTag.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  tagexist <- DB.isTagExist dbH (AddNewsTag.tag_id req)
  unless tagexist (throw ErrorTagNotExist)
  res <- DB.addNewsTag dbH
                       (AddNewsTag.news_id req)
                       (AddNewsTag.tag_id req)
                       (AddNewsTag.token req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

createNews :: DB.Handle -> CreateNews.Request -> Result CreateNews.NewsId
createNews dbH req = do
  author <- DB.isAuthor dbH (CreateNews.token req)
  unless author (throw ErrorNotAuthor)
  exist <- DB.isCategoryExist dbH (CreateNews.cat_id req)
  unless exist (throw ErrorCategoryNotExist)
  res <- DB.createNews dbH
                       (CreateNews.name req)
                       (CreateNews.token req)
                       (CreateNews.cat_id req)
                       (CreateNews.text req)
  case res of
    Just news -> return news
    _         -> throw ErrorBadRequest

deleteNews :: DB.Handle -> DeleteNews.Request -> Result String
deleteNews dbH req = do
  author <- DB.isAuthor dbH (DeleteNews.token req)
  unless author (throw ErrorNotAuthor)
  exist <- DB.isNewsExist dbH (DeleteNews.news_id req)
  unless exist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (DeleteNews.news_id req)
                                  (DeleteNews.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  maybephotos <- DB.deleteNews dbH
                               (DeleteNews.news_id req)
                               (DeleteNews.token req)
  case maybephotos of
    Just photos -> do
      mapM_ (DB.deleteFile dbH) photos
      return justOK
    _ -> throw ErrorBadRequest

deleteNewsComment :: DB.Handle -> DeleteNewsComment.Request -> Result String
deleteNewsComment dbH req = do
  admin <- DB.isAdmin dbH (DeleteNewsComment.token req)
  unless admin (throw ErrorNotFound)
  res <- DB.deleteNewsComment dbH (DeleteNewsComment.comment_id req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

deleteNewsPhoto :: DB.Handle -> DeleteNewsPhoto.Request -> Result String
deleteNewsPhoto dbH req = do
  author <- DB.isAuthor dbH (DeleteNewsPhoto.token req)
  unless author (throw ErrorNotAuthor)
  exist <- DB.isNewsExist dbH (DeleteNewsPhoto.news_id req)
  unless exist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (DeleteNewsPhoto.news_id req)
                                  (DeleteNewsPhoto.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  res <- DB.deleteNewsPhoto dbH
                            (DeleteNewsPhoto.photo_id req)
                            (DeleteNewsPhoto.news_id req)
                            (DeleteNewsPhoto.token req)
  case res of
    Just photo -> DB.deleteFile dbH photo >> return justOK
    _          -> throw ErrorBadRequest

deleteNewsTag :: DB.Handle -> DeleteNewsTag.Request -> Result String
deleteNewsTag dbH req = do
  author <- DB.isAuthor dbH (DeleteNewsTag.token req)
  unless author (throw ErrorNotAuthor)
  newsexist <- DB.isNewsExist dbH (DeleteNewsTag.news_id req)
  unless newsexist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (DeleteNewsTag.news_id req)
                                  (DeleteNewsTag.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  tagexist <- DB.isTagExist dbH (DeleteNewsTag.tag_id req)
  unless tagexist (throw ErrorTagNotExist)
  res <- DB.deleteNewsTag dbH
                          (DeleteNewsTag.tag_id req)
                          (DeleteNewsTag.news_id req)
                          (DeleteNewsTag.token req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

getDrafts :: DB.Handle -> GetDrafts.Request -> Result [GetDrafts.Draft]
getDrafts dbH req = do
  unless (isValidPage (GetDrafts.page req)) (throw ErrorBadPage)
  author <- DB.isAuthor dbH (GetDrafts.token req)
  unless author (throw ErrorNotAuthor)
  res <- DB.getDrafts dbH
                      (GetDrafts.page req)
                      (Constants.newsPerPage)
                      (GetDrafts.token req)
  case res of
    Just drafts -> return drafts
    _           -> throw ErrorBadRequest

getNews :: DB.Handle -> GetNews.Request -> Result [GetNews.News]
getNews dbH req = do
  unless (isValidPage (GetNews.page req)) (throw ErrorBadPage)
  maybenews <- DB.getNews dbH
                          (GetNews.created_at req)
                          (GetNews.created_before req)
                          (GetNews.created_after req)
                          (GetNews.author_contains req)
                          (GetNews.name_contains req)
                          (GetNews.text_contains req)
                          (GetNews.anything_contains req)
                          (GetNews.cat_id req)
                          (GetNews.tags_all req)
                          (GetNews.tags_any req)
                          (GetNews.sort_by req)
                          (GetNews.page req)
                          (Constants.newsPerPage)
  let news = maybe (throw ErrorBadRequest) id maybenews
  cats <- DB.getCategoriesAll dbH
  return $ map (buildAnswer cats) news
 where
  buildAnswer :: [GetCategories.Cat] -> GetNews.News -> GetNews.News
  buildAnswer cats t = t
    { GetNews.news_category = buildCategories cats $ GetNews.news_category_id t
    }
  buildCategories :: [GetCategories.Cat] -> Int -> GetNews.Category
  buildCategories cats c =
    let tempc = head $ filter (\i -> GetCategories.id i == c) cats
    in
      GetNews.Category
        { GetNews.category_id     = GetCategories.id tempc
        , GetNews.category_name   = GetCategories.name tempc
        , GetNews.category_parent = buildCategories cats
                                      <$> GetCategories.parent tempc
        }

getNewsComments
  :: DB.Handle -> GetNewsComments.Request -> Result [GetNewsComments.Comment]
getNewsComments dbH req = do
  unless (isValidPage (GetNewsComments.page req)) (throw ErrorBadPage)
  published <- DB.isNewsPublished dbH (GetNewsComments.news_id req)
  unless published (throw ErrorNewsNotExist)
  res <- DB.getNewsComments dbH
                            (GetNewsComments.news_id req)
                            (GetNewsComments.page req)
                            (Constants.commentsPerPage)
  case res of
    Just res -> return res
    _        -> throw ErrorBadRequest

publishNews :: DB.Handle -> PublishNews.Request -> Result String
publishNews dbH req = do
  author <- DB.isAuthor dbH (PublishNews.token req)
  unless author (throw ErrorNotAuthor)
  newsexist <- DB.isNewsExist dbH (PublishNews.news_id req)
  unless newsexist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (PublishNews.news_id req)
                                  (PublishNews.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  res <- DB.publishNews dbH
                        (PublishNews.publish req)
                        (PublishNews.news_id req)
                        (PublishNews.token req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

setNewsMainPhoto :: DB.Handle -> SetNewsMainPhoto.Request -> Result String
setNewsMainPhoto dbH req = do
  author <- DB.isAuthor dbH (SetNewsMainPhoto.token req)
  unless author (throw ErrorNotAuthor)
  newsexist <- DB.isNewsExist dbH (SetNewsMainPhoto.news_id req)
  unless newsexist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (SetNewsMainPhoto.news_id req)
                                  (SetNewsMainPhoto.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  oldphoto <- DB.getNewsMainPhoto dbH
                                  (SetNewsMainPhoto.news_id req)
                                  (SetNewsMainPhoto.token req)
  case oldphoto of
    Right (Just photo) -> DB.deleteFile dbH photo >> return ()
    Left  ()           -> throw ErrorInternal
    Right Nothing      -> return ()
  let ext = makeExt (SetNewsMainPhoto.photo_type req)
  res <- DB.setNewsMainPhoto dbH
                             (SetNewsMainPhoto.news_id req)
                             (SetNewsMainPhoto.token req)
                             ext
  case res of
    Just photo ->
      DB.saveImage dbH photo (SetNewsMainPhoto.photo req) >> return justOK
    _ -> throw ErrorBadRequest

updateNews :: DB.Handle -> UpdateNews.Request -> Result String
updateNews dbH req = do
  author <- DB.isAuthor dbH (UpdateNews.token req)
  unless author (throw ErrorNotAuthor)
  newsexist <- DB.isNewsExist dbH (UpdateNews.news_id req)
  unless newsexist (throw ErrorNewsNotExist)
  newsAuthor <- DB.thisNewsAuthor dbH
                                  (UpdateNews.news_id req)
                                  (UpdateNews.token req)
  unless newsAuthor (throw ErrorNotYourNews)
  catexist <- DB.isCategoryExist dbH (UpdateNews.cat_id req)
  unless catexist (throw ErrorCategoryNotExist)
  res <- DB.updateNews dbH
                       (UpdateNews.name req)
                       (UpdateNews.token req)
                       (UpdateNews.cat_id req)
                       (UpdateNews.text req)
                       (UpdateNews.news_id req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

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
