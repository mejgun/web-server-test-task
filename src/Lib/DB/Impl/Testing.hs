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
                      , DB.ifLoginNotExist     = ifLoginNotExist
                      , DB.ifLoginExist        = ifLoginExist
                      , DB.saveFile            = saveFile
                      }

createUser
  :: DB.Name -> DB.LastName -> DB.Login -> DB.Password -> DB.Result Bool
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
  -> DB.Result String
createUserWithPhoto name lastname login password ext =
  return $ case any null [name, lastname, login, password, ext] of
    True -> Nothing
    _    -> Just "photo"

getUsers :: DB.Page -> DB.Count -> DB.Result [GetUsers.User]
getUsers page count = return $ Just []

ifLoginNotExist :: DB.Login -> DB.Result Bool
ifLoginNotExist login = return $ if null login then Nothing else Just True

ifLoginExist :: DB.Login -> DB.Result Bool
ifLoginExist = undefined

saveFile :: FilePath -> String -> DB.Result Bool
saveFile file str = return $ case any null [file, str] of
  True -> Nothing
  _    -> Just True
