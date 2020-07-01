{-# LANGUAGE OverloadedStrings #-}

module Lib.DB.Impl.Testing
  ( newHandle
  )
where

import qualified Lib.Types.GetAuthors          as GetAuthors
import qualified Lib.Types.GetCategories       as GetCategories
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
  :: DB.Name -> DB.LastName -> DB.Login -> DB.Password -> DB.MaybeResult Bool
createUser name lastname login password =
  return $ case any null [name, lastname, login, password] of
    True -> Nothing
    _    -> Just True

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

deleteAuthor :: DB.Login -> DB.MaybeResult Bool
deleteAuthor _ = return $ Just True

editAuthor :: DB.Login -> DB.Description -> DB.MaybeResult Bool
editAuthor _ _ = return $ Just True

makeAuthor :: DB.Login -> DB.Description -> DB.MaybeResult Bool
makeAuthor _ _ = return $ Just True

createCategory :: DB.CategoryName -> DB.ParentCategory -> DB.MaybeResult Bool
createCategory _ _ = return $ Just True

deleteCategory :: DB.CategoryID -> DB.MaybeResult Bool
deleteCategory _ = return $ Just True

editCategory
  :: DB.CategoryID
  -> DB.CategoryName
  -> DB.ParentCategory
  -> DB.MaybeResult Bool
editCategory _ _ _ = return $ Just True

getCategories :: DB.Page -> DB.Count -> DB.MaybeResult [GetCategories.Cat]
getCategories _ _ = return $ Just []

createTag :: DB.TagName -> DB.MaybeResult Bool
createTag _ = return $ Just True

isLoginNotExist :: DB.Login -> DB.Result Bool
isLoginNotExist login = return $ login == "notexistlogin"

isLoginExist :: DB.Login -> DB.Result Bool
isLoginExist login = return $ login == "existlogin"

isAuthorExist :: DB.Login -> DB.Result Bool
isAuthorExist login = return $ login == "authorlogin"

isAdmin :: DB.Token -> DB.Result Bool
isAdmin login = return $ login == "admin"

isAuthor :: DB.Token -> DB.Result Bool
isAuthor login = return $ login == "author"

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
  return $ if (news_id > 0) && (token == "authortoken") then True else False

saveImage :: FilePath -> String -> DB.MaybeResult Bool
saveImage file str = return $ case any null [file, str] of
  True -> Nothing
  _    -> Just True

deleteFile :: FilePath -> DB.MaybeResult Bool
deleteFile file = return $ case null file of
  True -> Nothing
  _    -> Just True
