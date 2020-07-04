{-# LANGUAGE OverloadedStrings #-}

module Lib.Routes where

import           Network.Wai

import qualified Lib.DB                        as DB
                                                ( Handle )
import qualified Lib.Functions                 as Functions
import qualified Lib.Handlers                  as Handlers

runApp
  :: DB.Handle
  -> Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
runApp dbH request respond = case pathInfo request of
  ["user"    , "get"          ] -> norm Handlers.getUsers
  ["user"    , "create"       ] -> norm Handlers.createUser
  ["user"    , "delete"       ] -> adm Handlers.deleteUser
  ["user"    , "login"        ] -> norm Handlers.loginUser
  ["author"  , "make"         ] -> adm Handlers.makeAuthor
  ["author"  , "edit"         ] -> adm Handlers.editAuthor
  ["author"  , "delete"       ] -> adm Handlers.deleteAuthor
  ["author"  , "get"          ] -> adm Handlers.getAuthors
  ["category", "create"       ] -> adm Handlers.createCategory
  ["category", "edit"         ] -> adm Handlers.editCategory
  ["category", "delete"       ] -> adm Handlers.deleteCategory
  ["category", "get"          ] -> norm Handlers.getCategories
  ["tag"     , "create"       ] -> adm Handlers.createTag
  ["tag"     , "edit"         ] -> adm Handlers.editTag
  ["tag"     , "delete"       ] -> adm Handlers.deleteTag
  ["tag"     , "get"          ] -> norm Handlers.getTags
  ["news"    , "create"       ] -> norm Handlers.createNews
  ["news"    , "update"       ] -> norm Handlers.updateNews
  ["news"    , "publish"      ] -> norm Handlers.publishNews
  ["news"    , "setmainphoto" ] -> norm Handlers.setNewsMainPhoto
  ["news"    , "addphoto"     ] -> norm Handlers.addNewsPhoto
  ["news"    , "deletephoto"  ] -> norm Handlers.deleteNewsPhoto
  ["news"    , "addtag"       ] -> norm Handlers.addNewsTag
  ["news"    , "deletetag"    ] -> norm Handlers.deleteNewsTag
  ["news"    , "addcomment"   ] -> norm Handlers.addNewsComment
  ["news"    , "deletecomment"] -> adm Handlers.deleteNewsComment
  ["news"    , "getcomments"  ] -> norm Handlers.getNewsComments
  ["news"    , "delete"       ] -> norm Handlers.deleteNews
  ["news"    , "get"          ] -> norm Handlers.getNews
  ["news"    , "getdrafts"    ] -> norm Handlers.getDrafts
  ["images"  , img            ] -> Functions.returnFile img respond
  _                             -> Functions.return404 respond
 where
  norm x = Functions.normalHandler (x dbH) request respond
  adm x = Functions.adminHandler (x dbH) request respond
