module Lib.DB
  ( Handle(..)
  , Result
  , Name
  , LastName
  , Login
  , Password
  , PhotoExt
  , Page
  , Count
  )
where

import qualified Lib.Types.GetUsers            as GetUsers

type Name = String

type LastName = String

type Login = String

type Password = String

type Page = Int

type Count = Int

type PhotoExt = String

type Result a = IO (Maybe a)

data Handle =
  Handle
    { createUser :: Name -> LastName -> Login -> Password -> Result Bool
    , createUserWithPhoto :: Name -> LastName -> Login -> Password -> PhotoExt -> Result String
    , getUsers :: Page -> Count -> Result [GetUsers.User]
    , ifLoginNotExist :: Login -> Result Bool
    , ifLoginExist :: Login -> Result Bool
    , saveFile :: FilePath -> String -> Result Bool
    }
