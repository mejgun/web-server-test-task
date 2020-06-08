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
  ["user"    , "get"          ] -> f Users.get
  ["user"    , "create"       ] -> f Users.create
  ["user"    , "delete"       ] -> f Users.delete
  ["user"    , "login"        ] -> f Users.log_in
  ["author"  , "make"         ] -> f Authors.make
  ["author"  , "edit"         ] -> f Authors.edit
  ["author"  , "delete"       ] -> f Authors.delete
  ["author"  , "get"          ] -> f Authors.get
  ["category", "create"       ] -> f Categories.create
  ["category", "edit"         ] -> f Categories.edit
  ["category", "delete"       ] -> f Categories.delete
  ["category", "get"          ] -> f Categories.get
  ["tag"     , "create"       ] -> f Tags.create
  ["tag"     , "edit"         ] -> f Tags.edit
  ["tag"     , "delete"       ] -> f Tags.delete
  ["tag"     , "get"          ] -> f Tags.get
  ["news"    , "create"       ] -> f News.create
  ["news"    , "update"       ] -> f News.update
  ["news"    , "publish"      ] -> f News.release
  ["news"    , "setmainphoto" ] -> f News.setMainPhoto
  ["news"    , "addphoto"     ] -> f News.addPhoto
  ["news"    , "deletephoto"  ] -> f News.deletePhoto
  ["news"    , "addtag"       ] -> f News.addTag
  ["news"    , "deletetag"    ] -> f News.deleteTag
  ["news"    , "addcomment"   ] -> f News.addComment
  ["news"    , "deletecomment"] -> f News.deleteComment
  ["news"    , "getcomments"  ] -> f News.getComments
  ["news"    , "delete"       ] -> f News.delete
  ["news"    , "get"          ] -> f News.get
  ["images"  , img            ] -> returnFile img rd
  _                             -> rd responseERR
  where f x = rIfJsonBody x c r rd
