{-# LANGUAGE OverloadedStrings #-}

module Lib.Logic
  ( Handle(..)
  , ResultResponseError(..)
  , ResultResponse(..)
  , MyHandler
  , MyResult
  )
where

import           Control.Exception
import           Control.Monad.Except

import qualified Lib.Types.AddNewsComment
import qualified Lib.Types.AddNewsPhoto
import qualified Lib.Types.AddNewsTag
import qualified Lib.Types.CreateCategory
import qualified Lib.Types.CreateNews
import qualified Lib.Types.CreateTag
import qualified Lib.Types.CreateUser
import qualified Lib.Types.DeleteAuthor
import qualified Lib.Types.DeleteCategory
import qualified Lib.Types.DeleteNews
import qualified Lib.Types.DeleteNewsComment
import qualified Lib.Types.DeleteNewsPhoto
import qualified Lib.Types.DeleteNewsTag
import qualified Lib.Types.DeleteTag
import qualified Lib.Types.DeleteUser
import qualified Lib.Types.EditAuthor
import qualified Lib.Types.EditCategory
import qualified Lib.Types.EditTag
import qualified Lib.Types.GetAuthors
import qualified Lib.Types.GetCategories
import qualified Lib.Types.GetDrafts
import qualified Lib.Types.GetNews
import qualified Lib.Types.GetNewsComments
import qualified Lib.Types.GetTags
import qualified Lib.Types.GetUsers
import qualified Lib.Types.LoginUser
import qualified Lib.Types.MakeAuthor
import qualified Lib.Types.PublishNews
import qualified Lib.Types.SetNewsMainPhoto
import qualified Lib.Types.UpdateNews

data ResultResponse a
  = Error ResultResponseError
  | Success (Maybe a)

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

type MyResult b = IO (ResultResponse b)

type MyHandler a b = a -> MyResult b

data Handle =
  Handle
    { createUser :: MyHandler Lib.Types.CreateUser.Request Bool
    , getUsers :: MyHandler Lib.Types.GetUsers.Request [Lib.Types.GetUsers.User]
    , deleteUser :: MyHandler Lib.Types.DeleteUser.Request Bool
    , loginUser :: MyHandler Lib.Types.LoginUser.Request Lib.Types.LoginUser.Token
    , deleteAuthor :: MyHandler Lib.Types.DeleteAuthor.Request Bool
    , editAuthor :: MyHandler Lib.Types.EditAuthor.Request Bool
    , getAuthors :: MyHandler Lib.Types.GetAuthors.Request [Lib.Types.GetAuthors.Author]
    , makeAuthor :: MyHandler Lib.Types.MakeAuthor.Request Bool
    , createCategory :: MyHandler Lib.Types.CreateCategory.Request Bool
    , deleteCategory :: MyHandler Lib.Types.DeleteCategory.Request Bool
    , editCategory :: MyHandler Lib.Types.EditCategory.Request Bool
    , getCategories :: MyHandler Lib.Types.GetCategories.Request [Lib.Types.GetCategories.Cat]
    , createTag :: MyHandler Lib.Types.CreateTag.Request Bool
    , deleteTag :: MyHandler Lib.Types.DeleteTag.Request Bool
    , editTag :: MyHandler Lib.Types.EditTag.Request Bool
    , getTags :: MyHandler Lib.Types.GetTags.Request [Lib.Types.GetTags.Tag]
    , addNewsComment :: MyHandler Lib.Types.AddNewsComment.Request Bool
    , addNewsPhoto :: MyHandler Lib.Types.AddNewsPhoto.Request Bool
    , addNewsTag :: MyHandler Lib.Types.AddNewsTag.Request Bool
    , createNews :: MyHandler Lib.Types.CreateNews.Request Lib.Types.CreateNews.NewsId
    , deleteNews :: MyHandler Lib.Types.DeleteNews.Request Bool
    , deleteNewsComment :: MyHandler Lib.Types.DeleteNewsComment.Request Bool
    , deleteNewsPhoto :: MyHandler Lib.Types.DeleteNewsPhoto.Request Bool
    , deleteNewsTag :: MyHandler Lib.Types.DeleteNewsTag.Request Bool
    , getDrafts :: MyHandler Lib.Types.GetDrafts.Request [Lib.Types.GetDrafts.Draft]
    , getNews :: MyHandler Lib.Types.GetNews.Request [Lib.Types.GetNews.News]
    , getNewsComments :: MyHandler Lib.Types.GetNewsComments.Request [Lib.Types.GetNewsComments.Comment]
    , publishNews :: MyHandler Lib.Types.PublishNews.Request Bool
    , setNewsMainPhoto :: MyHandler Lib.Types.SetNewsMainPhoto.Request Bool
    , updateNews :: MyHandler Lib.Types.UpdateNews.Request Bool
    }
