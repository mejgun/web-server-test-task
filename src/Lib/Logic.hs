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

import qualified Lib.Requests.CreateUser
import qualified Lib.Requests.GetUsers

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
    { createUser :: MyHandler Lib.Requests.CreateUser.Request Bool
    , getUsers :: MyHandler Lib.Requests.GetUsers.Request [Lib.Requests.GetUsers.User]
    }
