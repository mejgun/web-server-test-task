module Lib.DB where

import qualified Lib.Types.GetAuthors          as GetAuthors
import qualified Lib.Types.GetCategories       as GetCategories
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

type CategoryName = String

type ParentCategory = Maybe Int

type TagName = String

type TagID = Int

type NewsID = Int

type MaybeResult a = IO (Maybe a)

type EitherResult a = IO (Either () (Maybe a))

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
    , makeAuthor :: Login -> Description -> MaybeResult Bool
    , getAuthors :: Page -> Count -> MaybeResult [GetAuthors.Author]
    , deleteCategory :: CategoryID -> MaybeResult Bool
    , createCategory :: CategoryName -> ParentCategory -> MaybeResult Bool
    , editCategory :: CategoryID -> CategoryName -> ParentCategory -> MaybeResult Bool
    , getCategories :: Page -> Count -> MaybeResult [GetCategories.Cat]
    , isLoginNotExist :: Login -> Result Bool
    , isLoginExist :: Login -> Result Bool
    , isAdmin :: Token -> Result Bool
    , isAuthor :: Token -> Result Bool
    , isUser :: Token -> Result Bool
    , isAuthorExist :: Login -> Result Bool
    , isTagNotExist :: TagName -> Result Bool
    , isCategoryExist :: CategoryID -> Result Bool
    , isTagExist :: TagID -> Result Bool
    , isNewsExist :: NewsID -> Result Bool
    , isNewsPublished :: NewsID -> Result Bool
    , thisNewsAuthor :: NewsID -> Token -> Result Bool
    , saveImage :: FilePath -> Base64String -> MaybeResult Bool
    , deleteFile :: FilePath -> MaybeResult Bool
    }
