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
                      , DB.ifLoginNotExist     = ifLoginNotExist
                      , DB.ifLoginExist        = ifLoginExist
                      , DB.isAdmin             = isAdmin
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

ifLoginNotExist :: DB.Login -> DB.Result Bool
ifLoginNotExist login = return $ login == "notexistlogin"

ifLoginExist :: DB.Login -> DB.Result Bool
ifLoginExist login = return $ login == "existlogin"

isAdmin :: DB.Token -> DB.Result Bool
isAdmin login = return $ login == "admin"

saveImage :: FilePath -> String -> DB.MaybeResult Bool
saveImage file str = return $ case any null [file, str] of
  True -> Nothing
  _    -> Just True

deleteFile :: FilePath -> DB.MaybeResult Bool
deleteFile file = return $ case null file of
  True -> Nothing
  _    -> Just True
