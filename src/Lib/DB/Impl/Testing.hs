{-# LANGUAGE OverloadedStrings #-}

module Lib.DB.Impl.Testing
  ( newHandle
  )
where

import qualified Lib.Types.GetUsers            as GetUsers

import qualified Lib.DB                        as DB

newHandle :: DB.Handle
newHandle = DB.Handle { DB.createUser          = createUser
                      , DB.createUserWithPhoto = createUserWithPhoto
                      , DB.getUsers            = getUsers
                      , DB.deleteUser          = deleteUser
                      , DB.deleteAuthor        = deleteAuthor
                      , DB.editAuthor          = editAuthor
                      , DB.loginUser           = loginUser
                      , DB.isLoginNotExist     = isLoginNotExist
                      , DB.isLoginExist        = isLoginExist
                      , DB.isAuthorExist       = isAuthorExist
                      , DB.isAdmin             = isAdmin
                      , DB.isAuthor            = isAuthor
                      , DB.isUser              = isUser
                      , DB.isCategoryExist     = isCategoryExist
                      , DB.isTagNotExist       = isTagNotExist
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
getUsers page count = return $ Just []

deleteUser :: DB.Login -> DB.EitherResult DB.PhotoPath
deleteUser _ = return $ Right $ Just "file"

loginUser :: DB.Login -> DB.Password -> DB.MaybeResult DB.Token
loginUser "login" "password" = return $ Just "token"
loginUser _       _          = return Nothing

deleteAuthor :: DB.Login -> DB.MaybeResult Bool
deleteAuthor _ = return $ Just True

editAuthor :: DB.Login -> DB.Description -> DB.MaybeResult Bool
editAuthor _ _ = return $ Just True

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
isTagNotExist "existtag"    = return False
isTagNotExist "notexisttag" = return True

saveImage :: FilePath -> String -> DB.MaybeResult Bool
saveImage file str = return $ case any null [file, str] of
  True -> Nothing
  _    -> Just True

deleteFile :: FilePath -> DB.MaybeResult Bool
deleteFile file = return $ case null file of
  True -> Nothing
  _    -> Just True
