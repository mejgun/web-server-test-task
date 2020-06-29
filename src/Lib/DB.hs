module Lib.DB where

import qualified Lib.Types.GetUsers            as GetUsers

type Token = String

type Base64String = String

type Name = String

type LastName = String

type Login = String

type Password = String

type Page = Int

type Count = Int

type PhotoExt = String

type PhotoPath = String

type Description = String

type CategoryID = Int

type TagName = String

type MaybeResult a = IO (Maybe a)

type EitherResult a = IO (Either Bool (Maybe a))

type Result a = IO a

data Handle =
  Handle
    { createUser :: Name -> LastName -> Login -> Password -> MaybeResult Bool
    , createUserWithPhoto :: Name -> LastName -> Login -> Password -> PhotoExt -> MaybeResult PhotoPath
    , getUsers :: Page -> Count -> MaybeResult [GetUsers.User]
    , deleteUser :: Login -> EitherResult PhotoPath
    , loginUser :: Login -> Password -> MaybeResult Token
    , deleteAuthor :: Login -> MaybeResult Bool
    , editAuthor :: Login -> Description -> MaybeResult Bool
    , isLoginNotExist :: Login -> Result Bool
    , isLoginExist :: Login -> Result Bool
    , isAdmin :: Token -> Result Bool
    , isAuthor :: Token -> Result Bool
    , isUser :: Token -> Result Bool
    , isAuthorExist :: Login -> Result Bool
    , isTagNotExist :: TagName -> Result Bool
    , isCategoryExist :: CategoryID -> Result Bool
    , saveImage :: FilePath -> Base64String -> MaybeResult Bool
    , deleteFile :: FilePath -> MaybeResult Bool
    }
