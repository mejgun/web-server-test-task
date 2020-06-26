module Lib.Logic
  ( Handle(..)
  , ExceptMonad
  , ResultResponseError(..)
  , MyHandler
  )
where

import           Control.Monad.Except

import qualified Lib.Requests.CreateUser
import qualified Lib.Requests.GetUsers

type ExceptMonad = ExceptT ResultResponseError IO

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

type MyHandler a b = a -> ExceptMonad b

data Handle =
  Handle
    { createUser :: Lib.Requests.CreateUser.Request -> ExceptMonad String
    , getUsers :: Lib.Requests.GetUsers.Request -> ExceptMonad [Lib.Requests.GetUsers.User]
    }
