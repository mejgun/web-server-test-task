{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes where

import           Network.Wai

import qualified Lib.DB                        as DB
import           Lib.Functions
import qualified Lib.Handlers                  as Handlers
import qualified Lib.Logger                    as Logger

runApp
  :: DB.Handle
  -> Logger.Logger
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
runApp dbH logger request respond = case pathInfo request of
  ["user"    , "get"       ] -> norm Handlers.getUsers
  ["user"    , "create"    ] -> norm Handlers.createUser
  ["user"    , "delete"    ] -> adm Handlers.deleteUser
  ["user"    , "login"     ] -> norm Handlers.loginUser
  ["author"  , "make"      ] -> adm Handlers.makeAuthor
  ["author"  , "edit"      ] -> adm Handlers.editAuthor
  ["author"  , "delete"    ] -> adm Handlers.deleteAuthor
  ["author"  , "get"       ] -> adm Handlers.getAuthors
  ["category", "create"    ] -> adm Handlers.createCategory
  ["category", "edit"      ] -> adm Handlers.editCategory
  ["category", "delete"    ] -> adm Handlers.deleteCategory
  ["category", "get"       ] -> norm Handlers.getCategories
  ["tag"     , "create"    ] -> adm Handlers.createTag
  ["tag"     , "edit"      ] -> adm Handlers.editTag
  ["tag"     , "delete"    ] -> adm Handlers.deleteTag
  ["tag"     , "get"       ] -> norm Handlers.getTags
  ["news"    , "create"    ] -> norm Handlers.createNews
--   ["news"    , "update"       ] -> norm News.update
--   ["news"    , "publish"      ] -> norm News.release
--   ["news"    , "setmainphoto" ] -> norm News.setMainPhoto
  ["news"    , "addphoto"  ] -> norm Handlers.addNewsPhoto
--   ["news"    , "deletephoto"  ] -> norm News.deletePhoto
  ["news"    , "addtag"    ] -> norm Handlers.addNewsTag
--   ["news"    , "deletetag"    ] -> norm News.deleteTag
  ["news"    , "addcomment"] -> norm Handlers.addNewsComment
--   ["news"    , "deletecomment"] -> adm News.deleteComment
--   ["news"    , "getcomments"  ] -> norm News.getComments
  ["news"    , "delete"    ] -> norm Handlers.deleteNews
--   ["news"    , "get"          ] -> norm News.get
--   ["news"    , "getdrafts"    ] -> norm News.getDrafts
  ["images"  , img         ] -> returnFile img respond
  _                          -> return404 respond
 where
  norm x = normalHandler (x dbH) request respond
  adm x = adminHandler (x dbH) request respond
