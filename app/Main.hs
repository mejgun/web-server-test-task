{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )

import qualified Authors
import qualified Categories
import qualified News
import           PG
import qualified Tags
import           Types
import qualified Users

main :: IO ()
main = do
  conn <- pgconnect
  createImagesDir
  putStrLn "Server started"
  run 8080 $ logStdout $ application conn

application :: MyApp
application c r rd = case pathInfo r of
  ["user"    , "get"          ] -> norm Users.get
  ["user"    , "create"       ] -> norm Users.create
  ["user"    , "delete"       ] -> adm Users.delete
  ["user"    , "login"        ] -> norm Users.log_in
  ["author"  , "make"         ] -> adm Authors.make
  ["author"  , "edit"         ] -> adm Authors.edit
  ["author"  , "delete"       ] -> adm Authors.delete
  ["author"  , "get"          ] -> adm Authors.get
  ["category", "create"       ] -> adm Categories.create
  ["category", "edit"         ] -> adm Categories.edit
  ["category", "delete"       ] -> adm Categories.delete
  ["category", "get"          ] -> norm Categories.get
  ["tag"     , "create"       ] -> adm Tags.create
  ["tag"     , "edit"         ] -> adm Tags.edit
  ["tag"     , "delete"       ] -> adm Tags.delete
  ["tag"     , "get"          ] -> norm Tags.get
  ["news"    , "create"       ] -> norm News.create
  ["news"    , "update"       ] -> norm News.update
  ["news"    , "publish"      ] -> norm News.release
  ["news"    , "setmainphoto" ] -> norm News.setMainPhoto
  ["news"    , "addphoto"     ] -> norm News.addPhoto
  ["news"    , "deletephoto"  ] -> norm News.deletePhoto
  ["news"    , "addtag"       ] -> norm News.addTag
  ["news"    , "deletetag"    ] -> norm News.deleteTag
  ["news"    , "addcomment"   ] -> norm News.addComment
  ["news"    , "deletecomment"] -> adm News.deleteComment
  ["news"    , "getcomments"  ] -> norm News.getComments
  ["news"    , "delete"       ] -> norm News.delete
  ["news"    , "get"          ] -> norm News.get
  ["news"    , "getdrafts"    ] -> norm News.getDrafts
  ["images"  , img            ] -> returnFile img rd
  _                             -> return404 rd
 where
  norm x = normalHandler x c r rd
  adm x = adminHandler x c r rd
