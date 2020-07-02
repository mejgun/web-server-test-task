{-# LANGUAGE OverloadedStrings #-}

module Lib.DB.Impl.Testing
  ( newHandle
  )
where

import qualified Lib.Types.CreateNews          as CreateNews
import qualified Lib.Types.GetAuthors          as GetAuthors
import qualified Lib.Types.GetCategories       as GetCategories
import qualified Lib.Types.GetTags             as GetTags
import qualified Lib.Types.GetUsers            as GetUsers

import qualified Lib.DB                        as DB

newHandle :: DB.Handle
newHandle = DB.Handle { DB.createUser          = createUser
                      , DB.createUserWithPhoto = createUserWithPhoto
                      , DB.getUsers            = getUsers
                      , DB.getAuthors          = getAuthors
                      , DB.deleteUser          = deleteUser
                      , DB.deleteAuthor        = deleteAuthor
                      , DB.editAuthor          = editAuthor
                      , DB.makeAuthor          = makeAuthor
                      , DB.createCategory      = createCategory
                      , DB.loginUser           = loginUser
                      , DB.deleteCategory      = deleteCategory
                      , DB.editCategory        = editCategory
                      , DB.getCategories       = getCategories
                      , DB.createTag           = createTag
                      , DB.deleteTag           = deleteTag
                      , DB.editTag             = editTag
                      , DB.getTags             = getTags
                      , DB.addNewsComment      = addNewsComment
                      , DB.addNewsPhoto        = addNewsPhoto
                      , DB.addNewsTag          = addNewsTag
                      , DB.createNews          = createNews
                      , DB.deleteNews          = deleteNews
                      , DB.deleteNewsComment   = deleteNewsComment
                      , DB.deleteNewsPhoto     = deleteNewsPhoto
                      , DB.deleteNewsTag       = deleteNewsTag
                      , DB.isLoginNotExist     = isLoginNotExist
                      , DB.isLoginExist        = isLoginExist
                      , DB.isAuthorExist       = isAuthorExist
                      , DB.isAdmin             = isAdmin
                      , DB.isAuthor            = isAuthor
                      , DB.isUser              = isUser
                      , DB.isCategoryExist     = isCategoryExist
                      , DB.isTagNotExist       = isTagNotExist
                      , DB.isTagExist          = isTagExist
                      , DB.isNewsExist         = isNewsExist
                      , DB.isNewsPublished     = isNewsPublished
                      , DB.thisNewsAuthor      = thisNewsAuthor
                      , DB.saveImage           = saveImage
                      , DB.deleteFile          = deleteFile
                      }

createUser
  :: DB.Name -> DB.LastName -> DB.Login -> DB.Password -> DB.MaybeResult ()
createUser name lastname login password =
  return $ case any null [name, lastname, login, password] of
    True -> Nothing
    _    -> Just ()

createUserWithPhoto
  :: DB.Name
  -> DB.LastName
  -> DB.Login
  -> DB.Password
  -> DB.PhotoExt
  -> DB.MaybeResult String
createUserWithPhoto name lastname login password ext =
  return $ case any null [name, lastname, login, password, ext] of
    True -> Nothing
    _    -> Just "photo"

getUsers :: DB.Page -> DB.Count -> DB.MaybeResult [GetUsers.User]
getUsers _ _ = return $ Just []

getAuthors :: DB.Page -> DB.Count -> DB.MaybeResult [GetAuthors.Author]
getAuthors _ _ = return $ Just []

deleteUser :: DB.Login -> DB.EitherResult DB.PhotoPath
deleteUser _ = return $ Right $ Just "file"

loginUser :: DB.Login -> DB.Password -> DB.MaybeResult DB.Token
loginUser "login" "password" = return $ Just "token"
loginUser _       _          = return Nothing

deleteAuthor :: DB.Login -> DB.MaybeResult ()
deleteAuthor _ = return $ Just ()

editAuthor :: DB.Login -> DB.Description -> DB.MaybeResult ()
editAuthor _ _ = return $ Just ()

makeAuthor :: DB.Login -> DB.Description -> DB.MaybeResult ()
makeAuthor _ _ = return $ Just ()

createCategory :: DB.CategoryName -> DB.ParentCategory -> DB.MaybeResult ()
createCategory _ _ = return $ Just ()

deleteCategory :: DB.CategoryID -> DB.MaybeResult ()
deleteCategory _ = return $ Just ()

editCategory
  :: DB.CategoryID -> DB.CategoryName -> DB.ParentCategory -> DB.MaybeResult ()
editCategory _ _ _ = return $ Just ()

getCategories :: DB.Page -> DB.Count -> DB.MaybeResult [GetCategories.Cat]
getCategories _ _ = return $ Just []

createTag :: DB.TagName -> DB.MaybeResult ()
createTag _ = return $ Just ()

deleteTag :: DB.TagID -> DB.MaybeResult ()
deleteTag _ = return $ Just ()

editTag :: DB.TagID -> DB.TagName -> DB.MaybeResult ()
editTag _ _ = return $ Just ()

getTags :: DB.Page -> DB.Count -> DB.MaybeResult [GetTags.Tag]
getTags _ _ = return $ Just []

addNewsComment :: DB.NewsID -> DB.CommentText -> DB.Token -> DB.MaybeResult ()
addNewsComment _ _ _ = return $ Just ()

addNewsPhoto
  :: DB.NewsID -> DB.Token -> DB.PhotoExt -> DB.MaybeResult DB.PhotoPath
addNewsPhoto _ _ _ = return $ Just "photopath"

addNewsTag :: DB.NewsID -> DB.TagID -> DB.Token -> DB.MaybeResult ()
addNewsTag _ _ _ = return $ Just ()

createNews
  :: DB.NewsName
  -> DB.Token
  -> DB.CategoryID
  -> DB.NewsText
  -> DB.MaybeResult CreateNews.NewsId
createNews _ _ _ _ = return $ Just CreateNews.NewsId { CreateNews.news_id = 1 }

deleteNews :: DB.NewsID -> DB.Token -> DB.MaybeResult [DB.PhotoPath]
deleteNews _ _ = return $ Just []

deleteNewsComment :: DB.CommentID -> DB.MaybeResult ()
deleteNewsComment _ = return $ Just ()

deleteNewsPhoto
  :: DB.PhotoID -> DB.NewsID -> DB.Token -> DB.MaybeResult DB.PhotoPath
deleteNewsPhoto _ _ _ = return $ Just "photo"

deleteNewsTag :: DB.TagID -> DB.NewsID -> DB.Token -> DB.MaybeResult ()
deleteNewsTag _ _ _ = return $ Just ()

isLoginNotExist :: DB.Login -> DB.Result Bool
isLoginNotExist login = return $ login == "notexistlogin"

isLoginExist :: DB.Login -> DB.Result Bool
isLoginExist login = return $ login == "existlogin"

isAuthorExist :: DB.Login -> DB.Result Bool
isAuthorExist login = return $ login == "authorlogin"

isAdmin :: DB.Token -> DB.Result Bool
isAdmin login = return $ login == "admin"

isAuthor :: DB.Token -> DB.Result Bool
isAuthor login = return $ login == "author" || login == "author2"

isUser :: DB.Token -> DB.Result Bool
isUser login = return $ login == "user"

isCategoryExist :: DB.CategoryID -> DB.Result Bool
isCategoryExist catID = return $ catID > 0

isTagNotExist :: DB.TagName -> DB.Result Bool
isTagNotExist "existtag" = return False
isTagNotExist _          = return True

isTagExist :: DB.TagID -> DB.Result Bool
isTagExist tag_id = return $ tag_id > 0

isNewsExist :: DB.NewsID -> DB.Result Bool
isNewsExist news_id = return $ news_id > 0

isNewsPublished :: DB.NewsID -> DB.Result Bool
isNewsPublished news_id = return $ news_id > 0

thisNewsAuthor :: DB.NewsID -> DB.Token -> DB.Result Bool
thisNewsAuthor news_id token =
  return $ if (news_id > 0) && (token == "author") then True else False

saveImage :: FilePath -> DB.Base64String -> DB.Result Bool
saveImage file str = return $ case any null [file, str] of
  True -> False
  _    -> True

deleteFile :: FilePath -> DB.Result Bool
deleteFile file = return $ case null file of
  True -> False
  _    -> True
