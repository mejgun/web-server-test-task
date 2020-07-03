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
import           Control.Monad                  ( unless )
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
        Just fileName ->
          saveImage dbH fileName (fromJust (CreateUser.photo req))
            >> return justOK
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
  checkIfValidPage (GetUsers.page req)
  r <- DB.getUsers dbH (GetUsers.page req) Constants.usersPerPage
  case r of
    Just users -> return users
    _          -> throw ErrorBadRequest

deleteUser :: DB.Handle -> DeleteUser.Request -> Result String
deleteUser dbH req = do
  checkIfAdmin dbH (DeleteUser.token req)
  checkIfLoginExist dbH (DeleteUser.login req)
  photo <- DB.deleteUser dbH (DeleteUser.login req)
  case photo of
    Right (Just f) -> deleteFile dbH f >> return justOK
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
  checkIfAdmin dbH (DeleteAuthor.token req)
  checkIfAuthorExist dbH (DeleteAuthor.login req)
  res <- DB.deleteAuthor dbH (DeleteAuthor.login req)
  if isJust res then return justOK else throw ErrorBadRequest

editAuthor :: DB.Handle -> EditAuthor.Request -> Result String
editAuthor dbH req = do
  checkIfAdmin dbH (EditAuthor.token req)
  checkIfAuthorExist dbH (EditAuthor.login req)
  res <- DB.editAuthor dbH (EditAuthor.login req) (EditAuthor.descr req)
  if isJust res then return justOK else throw ErrorBadRequest

getAuthors :: DB.Handle -> GetAuthors.Request -> Result [GetAuthors.Author]
getAuthors dbH req = do
  checkIfAdmin dbH (GetAuthors.token req)
  checkIfValidPage (GetAuthors.page req)
  res <- DB.getAuthors dbH (GetAuthors.page req) Constants.authorsPerPage
  case res of
    Just authors -> return authors
    _            -> throw ErrorBadRequest

makeAuthor :: DB.Handle -> MakeAuthor.Request -> Result String
makeAuthor dbH req = do
  checkIfAdmin dbH (MakeAuthor.token req)
  checkIfLoginExist dbH (MakeAuthor.login req)
  res <- DB.makeAuthor dbH (MakeAuthor.login req) (MakeAuthor.descr req)
  if isJust res then return justOK else throw ErrorBadRequest

createCategory :: DB.Handle -> CreateCategory.Request -> Result String
createCategory dbH req = do
  checkIfAdmin dbH (CreateCategory.token req)
  res <- DB.createCategory dbH
                           (CreateCategory.name req)
                           (CreateCategory.parent req)
  if isJust res then return justOK else throw ErrorBadRequest

deleteCategory :: DB.Handle -> DeleteCategory.Request -> Result String
deleteCategory dbH req = do
  checkIfAdmin dbH (DeleteCategory.token req)
  checkIfCategoryExist dbH (DeleteCategory.cat_id req)
  res <- DB.deleteCategory dbH (DeleteCategory.cat_id req)
  if isJust res then return justOK else throw ErrorBadRequest

editCategory :: DB.Handle -> EditCategory.Request -> Result String
editCategory dbH req = do
  checkIfAdmin dbH (EditCategory.token req)
  checkIfCategoryExist dbH (EditCategory.cat_id req)
  res <- DB.editCategory dbH
                         (EditCategory.cat_id req)
                         (EditCategory.name req)
                         (EditCategory.parent req)
  if isJust res then return justOK else throw ErrorBadRequest

getCategories
  :: DB.Handle -> GetCategories.Request -> Result [GetCategories.Cat]
getCategories dbH req = do
  checkIfValidPage (GetCategories.page req)
  res <- DB.getCategories dbH
                          (GetCategories.page req)
                          Constants.categoriesPerPage
  case res of
    Just categories -> return categories
    _               -> throw ErrorBadRequest

createTag :: DB.Handle -> CreateTag.Request -> Result String
createTag dbH req = do
  checkIfAdmin dbH (CreateTag.token req)
  notexist <- DB.isTagNotExist dbH (CreateTag.name req)
  unless notexist (throw ErrorTagAlreadyExist)
  res <- DB.createTag dbH (CreateTag.name req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

deleteTag :: DB.Handle -> DeleteTag.Request -> Result String
deleteTag dbH req = do
  checkIfAdmin dbH (DeleteTag.token req)
  checkIfTagExist dbH (DeleteTag.tag_id req)
  res <- DB.deleteTag dbH (DeleteTag.tag_id req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

editTag :: DB.Handle -> EditTag.Request -> Result String
editTag dbH req = do
  checkIfAdmin dbH (EditTag.token req)
  checkIfTagExist dbH (EditTag.tag_id req)
  res <- DB.editTag dbH (EditTag.tag_id req) (EditTag.name req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

getTags :: DB.Handle -> GetTags.Request -> Result [GetTags.Tag]
getTags dbH req = do
  checkIfValidPage (GetTags.page req)
  res <- DB.getTags dbH (GetTags.page req) Constants.tagsPerPage
  case res of
    Just tags -> return tags
    _         -> throw ErrorBadRequest

addNewsComment :: DB.Handle -> AddNewsComment.Request -> Result String
addNewsComment dbH req = do
  checkIfNewsPublished dbH (AddNewsComment.news_id req)
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
  checkIfAuthor dbH (AddNewsPhoto.token req)
  checkIfNewsExist dbH (AddNewsPhoto.news_id req)
  checkIfThisNewsAuthor dbH (AddNewsPhoto.news_id req) (AddNewsPhoto.token req)
  let ext = makeExt (AddNewsPhoto.photo_type req)
  res <- DB.addNewsPhoto dbH
                         (AddNewsPhoto.news_id req)
                         (AddNewsPhoto.token req)
                         ext
  case res of
    Just imgFile ->
      saveImage dbH imgFile (AddNewsPhoto.photo req) >> return justOK
    _ -> throw ErrorBadRequest

addNewsTag :: DB.Handle -> AddNewsTag.Request -> Result String
addNewsTag dbH req = do
  checkIfAuthor dbH (AddNewsTag.token req)
  checkIfNewsExist dbH (AddNewsTag.news_id req)
  checkIfThisNewsAuthor dbH (AddNewsTag.news_id req) (AddNewsTag.token req)
  checkIfTagExist dbH (AddNewsTag.tag_id req)
  res <- DB.addNewsTag dbH
                       (AddNewsTag.news_id req)
                       (AddNewsTag.tag_id req)
                       (AddNewsTag.token req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

createNews :: DB.Handle -> CreateNews.Request -> Result CreateNews.NewsId
createNews dbH req = do
  checkIfAuthor dbH (CreateNews.token req)
  checkIfCategoryExist dbH (CreateNews.cat_id req)
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
  checkIfAuthor dbH (DeleteNews.token req)
  checkIfNewsExist dbH (DeleteNews.news_id req)
  checkIfThisNewsAuthor dbH (DeleteNews.news_id req) (DeleteNews.token req)
  maybephotos <- DB.deleteNews dbH
                               (DeleteNews.news_id req)
                               (DeleteNews.token req)
  case maybephotos of
    Just photos -> do
      mapM_ (deleteFile dbH) photos
      return justOK
    _ -> throw ErrorBadRequest

deleteNewsComment :: DB.Handle -> DeleteNewsComment.Request -> Result String
deleteNewsComment dbH req = do
  checkIfAdmin dbH (DeleteNewsComment.token req)
  res <- DB.deleteNewsComment dbH (DeleteNewsComment.comment_id req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

deleteNewsPhoto :: DB.Handle -> DeleteNewsPhoto.Request -> Result String
deleteNewsPhoto dbH req = do
  checkIfAuthor dbH (DeleteNewsPhoto.token req)
  checkIfNewsExist dbH (DeleteNewsPhoto.news_id req)
  checkIfThisNewsAuthor dbH
                        (DeleteNewsPhoto.news_id req)
                        (DeleteNewsPhoto.token req)
  res <- DB.deleteNewsPhoto dbH
                            (DeleteNewsPhoto.photo_id req)
                            (DeleteNewsPhoto.news_id req)
                            (DeleteNewsPhoto.token req)
  case res of
    Just photo -> deleteFile dbH photo >> return justOK
    _          -> throw ErrorBadRequest

deleteNewsTag :: DB.Handle -> DeleteNewsTag.Request -> Result String
deleteNewsTag dbH req = do
  checkIfAuthor dbH (DeleteNewsTag.token req)
  checkIfNewsExist dbH (DeleteNewsTag.news_id req)
  checkIfThisNewsAuthor dbH
                        (DeleteNewsTag.news_id req)
                        (DeleteNewsTag.token req)
  checkIfTagExist dbH (DeleteNewsTag.tag_id req)
  res <- DB.deleteNewsTag dbH
                          (DeleteNewsTag.tag_id req)
                          (DeleteNewsTag.news_id req)
                          (DeleteNewsTag.token req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

getDrafts :: DB.Handle -> GetDrafts.Request -> Result [GetDrafts.Draft]
getDrafts dbH req = do
  checkIfValidPage (GetDrafts.page req)
  checkIfAuthor dbH (GetDrafts.token req)
  res <- DB.getDrafts dbH
                      (GetDrafts.page req)
                      (Constants.newsPerPage)
                      (GetDrafts.token req)
  case res of
    Just drafts -> return drafts
    _           -> throw ErrorBadRequest

getNews :: DB.Handle -> GetNews.Request -> Result [GetNews.News]
getNews dbH req = do
  checkIfValidPage (GetNews.page req)
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
  checkIfValidPage (GetNewsComments.page req)
  checkIfNewsPublished dbH (GetNewsComments.news_id req)
  res <- DB.getNewsComments dbH
                            (GetNewsComments.news_id req)
                            (GetNewsComments.page req)
                            (Constants.commentsPerPage)
  case res of
    Just comments -> return comments
    _             -> throw ErrorBadRequest

publishNews :: DB.Handle -> PublishNews.Request -> Result String
publishNews dbH req = do
  checkIfAuthor dbH (PublishNews.token req)
  checkIfNewsExist dbH (PublishNews.news_id req)
  checkIfThisNewsAuthor dbH (PublishNews.news_id req) (PublishNews.token req)
  res <- DB.publishNews dbH
                        (PublishNews.publish req)
                        (PublishNews.news_id req)
                        (PublishNews.token req)
  case res of
    Just () -> return justOK
    _       -> throw ErrorBadRequest

setNewsMainPhoto :: DB.Handle -> SetNewsMainPhoto.Request -> Result String
setNewsMainPhoto dbH req = do
  checkIfAuthor dbH (SetNewsMainPhoto.token req)
  checkIfNewsExist dbH (SetNewsMainPhoto.news_id req)
  checkIfThisNewsAuthor dbH
                        (SetNewsMainPhoto.news_id req)
                        (SetNewsMainPhoto.token req)
  oldphoto <- DB.getNewsMainPhoto dbH
                                  (SetNewsMainPhoto.news_id req)
                                  (SetNewsMainPhoto.token req)
  case oldphoto of
    Right (Just photo) -> deleteFile dbH photo >> return ()
    Left  ()           -> throw ErrorInternal
    Right Nothing      -> return ()
  let ext = makeExt (SetNewsMainPhoto.photo_type req)
  res <- DB.setNewsMainPhoto dbH
                             (SetNewsMainPhoto.news_id req)
                             (SetNewsMainPhoto.token req)
                             ext
  case res of
    Just photo ->
      saveImage dbH photo (SetNewsMainPhoto.photo req) >> return justOK
    _ -> throw ErrorBadRequest

updateNews :: DB.Handle -> UpdateNews.Request -> Result String
updateNews dbH req = do
  checkIfAuthor dbH (UpdateNews.token req)
  checkIfNewsExist dbH (UpdateNews.news_id req)
  checkIfThisNewsAuthor dbH (UpdateNews.news_id req) (UpdateNews.token req)
  checkIfCategoryExist dbH (UpdateNews.cat_id req)
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

checkIfAdmin :: DB.Handle -> DB.Token -> DB.Result ()
checkIfAdmin dbH token = do
  admin <- DB.isAdmin dbH token
  unless admin (throw ErrorNotFound)

checkIfAuthor :: DB.Handle -> DB.Token -> DB.Result ()
checkIfAuthor dbH token = do
  author <- DB.isAuthor dbH token
  unless author (throw ErrorNotAuthor)

checkIfValidPage :: DB.Page -> DB.Result ()
checkIfValidPage page = do
  unless (isValidPage page) (throw ErrorBadPage)

checkIfNewsExist :: DB.Handle -> DB.NewsID -> DB.Result ()
checkIfNewsExist dbH news_id = do
  exist <- DB.isNewsExist dbH news_id
  unless exist (throw ErrorNewsNotExist)

checkIfThisNewsAuthor :: DB.Handle -> DB.NewsID -> DB.Token -> DB.Result ()
checkIfThisNewsAuthor dbH news_id token = do
  newsAuthor <- DB.thisNewsAuthor dbH news_id token
  unless newsAuthor (throw ErrorNotYourNews)

checkIfCategoryExist :: DB.Handle -> DB.CategoryID -> DB.Result ()
checkIfCategoryExist dbH cat_id = do
  exist <- DB.isCategoryExist dbH cat_id
  unless exist (throw ErrorCategoryNotExist)

checkIfNewsPublished :: DB.Handle -> DB.NewsID -> DB.Result ()
checkIfNewsPublished dbH news_id = do
  published <- DB.isNewsPublished dbH news_id
  unless published (throw ErrorNewsNotExist)

checkIfLoginExist :: DB.Handle -> DB.Login -> DB.Result ()
checkIfLoginExist dbH login = do
  exist <- DB.isLoginExist dbH login
  unless exist (throw ErrorLoginNotExist)

checkIfAuthorExist :: DB.Handle -> DB.Login -> DB.Result ()
checkIfAuthorExist dbH login = do
  exist <- DB.isAuthorExist dbH login
  unless exist (throw ErrorAuthorNotExist)

checkIfTagExist :: DB.Handle -> DB.TagID -> DB.Result ()
checkIfTagExist dbH tag_id = do
  exist <- DB.isTagExist dbH tag_id
  unless exist (throw ErrorTagNotExist)

deleteFile :: DB.Handle -> FilePath -> DB.Result ()
deleteFile dbH file = do
  ok <- DB.deleteFile dbH file
  unless ok (throw ErrorInternal)

saveImage :: DB.Handle -> FilePath -> DB.Base64String -> DB.Result ()
saveImage dbH path photo = do
  ok <- DB.saveImage dbH path photo
  unless ok (throw ErrorInternal)
