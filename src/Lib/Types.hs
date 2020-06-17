module Lib.Types where

import           Network.Wai

import           Lib.Config

type MyApp
  =  Connection
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived

type MyHandler a b = Connection -> a -> IO (ResultResponse b)

data ResultResponse a = Ok200
    | OkJSON a
    | Error404
    | ErrorBadRequest
    | ErrorNotAuthor
    | ErrorLoginNotExist
    | ErrorLoginAlreadyExist
    | ErrorBadPage
    | ErrorAuthorNotExist
    | ErrorCategoryNotExist
    | ErrorTagAlreadyExist
    | ErrorTagNotExist
    | ErrorNewsNotExist
    | ErrorNotYourNews
    | ErrorNotUser
    deriving Show
