module Lib.DB where

import qualified Lib.Types.CreateNews          as CreateNews
import qualified Lib.Types.GetAuthors          as GetAuthors
import qualified Lib.Types.GetCategories       as GetCategories
import qualified Lib.Types.GetDrafts           as GetDrafts
import qualified Lib.Types.GetNewsComments     as GetNewsComments
import qualified Lib.Types.GetTags             as GetTags
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

type PhotoID = Int

type Description = String

type CategoryID = Int

type CategoryName = String

type ParentCategory = Maybe Int

type TagName = String

type TagID = Int

type NewsID = Int

type NewsName = String

type NewsText = String

type CommentText = String

type CommentID = Int

type PublishNews = Bool

type MaybeResult a = IO (Maybe a)

type EitherResult a = IO (Either () (Maybe a))

type Result a = IO a

data Handle =
  Handle
    { createUser :: Name -> LastName -> Login -> Password -> MaybeResult ()
    , createUserWithPhoto :: Name -> LastName -> Login -> Password -> PhotoExt -> MaybeResult PhotoPath
    , getUsers :: Page -> Count -> MaybeResult [GetUsers.User]
    , deleteUser :: Login -> EitherResult PhotoPath
    , loginUser :: Login -> Password -> MaybeResult Token
    , deleteAuthor :: Login -> MaybeResult ()
    , editAuthor :: Login -> Description -> MaybeResult ()
    , makeAuthor :: Login -> Description -> MaybeResult ()
    , getAuthors :: Page -> Count -> MaybeResult [GetAuthors.Author]
    , deleteCategory :: CategoryID -> MaybeResult ()
    , createCategory :: CategoryName -> ParentCategory -> MaybeResult ()
    , editCategory :: CategoryID -> CategoryName -> ParentCategory -> MaybeResult ()
    , getCategories :: Page -> Count -> MaybeResult [GetCategories.Cat]
    , createTag :: TagName -> MaybeResult ()
    , deleteTag :: TagID -> MaybeResult ()
    , editTag :: TagID -> TagName -> MaybeResult ()
    , getTags :: Page -> Count -> MaybeResult [GetTags.Tag]
    , addNewsComment :: NewsID -> CommentText -> Token -> MaybeResult ()
    , addNewsPhoto :: NewsID -> Token -> PhotoExt -> MaybeResult PhotoPath
    , addNewsTag :: NewsID -> TagID -> Token -> MaybeResult ()
    , createNews :: NewsName -> Token -> CategoryID -> NewsText -> MaybeResult CreateNews.NewsId
    , deleteNews :: NewsID -> Token -> MaybeResult [PhotoPath]
    , deleteNewsComment :: CommentID -> MaybeResult ()
    , deleteNewsPhoto :: PhotoID -> NewsID -> Token -> MaybeResult PhotoPath
    , deleteNewsTag :: TagID -> NewsID -> Token -> MaybeResult ()
    , getNewsMainPhoto :: NewsID -> Token -> EitherResult PhotoPath
    , setNewsMainPhoto :: NewsID -> Token -> PhotoExt -> MaybeResult PhotoPath
    , publishNews :: PublishNews -> NewsID -> Token -> MaybeResult ()
    , updateNews :: NewsName -> Token -> CategoryID -> NewsText -> NewsID -> MaybeResult ()
    , getNewsComments :: NewsID -> Page -> Count -> MaybeResult [GetNewsComments.Comment]
    , getDrafts :: Page -> Count -> Token -> MaybeResult [GetDrafts.Draft]
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
    , saveImage :: FilePath -> Base64String -> Result Bool
    , deleteFile :: FilePath -> Result Bool
    }
