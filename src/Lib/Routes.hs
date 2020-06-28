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
  ["user"  , "get"   ] -> norm $ Handlers.getUsers
  ["user"  , "create"] -> norm $ Handlers.createUser
--   ["user"    , "delete"       ] -> adm Users.delete
--   ["user"    , "login"        ] -> norm Users.logIn
--   ["author"  , "make"         ] -> adm Authors.make
--   ["author"  , "edit"         ] -> adm Authors.edit
--   ["author"  , "delete"       ] -> adm Authors.delete
--   ["author"  , "get"          ] -> adm Authors.get
--   ["category", "create"       ] -> adm Categories.create
--   ["category", "edit"         ] -> adm Categories.edit
--   ["category", "delete"       ] -> adm Categories.delete
--   ["category", "get"          ] -> norm Categories.get
--   ["tag"     , "create"       ] -> adm Tags.create
--   ["tag"     , "edit"         ] -> adm Tags.edit
--   ["tag"     , "delete"       ] -> adm Tags.delete
--   ["tag"     , "get"          ] -> norm Tags.get
--   ["news"    , "create"       ] -> norm News.create
--   ["news"    , "update"       ] -> norm News.update
--   ["news"    , "publish"      ] -> norm News.release
--   ["news"    , "setmainphoto" ] -> norm News.setMainPhoto
--   ["news"    , "addphoto"     ] -> norm News.addPhoto
--   ["news"    , "deletephoto"  ] -> norm News.deletePhoto
--   ["news"    , "addtag"       ] -> norm News.addTag
--   ["news"    , "deletetag"    ] -> norm News.deleteTag
--   ["news"    , "addcomment"   ] -> norm News.addComment
--   ["news"    , "deletecomment"] -> adm News.deleteComment
--   ["news"    , "getcomments"  ] -> norm News.getComments
--   ["news"    , "delete"       ] -> norm News.delete
--   ["news"    , "get"          ] -> norm News.get
--   ["news"    , "getdrafts"    ] -> norm News.getDrafts
  ["images", img     ] -> returnFile img respond
  _                    -> return404 respond
 where
  norm x = normalHandler (x dbH) request respond
  adm x = adminHandler (x dbH) request respond
