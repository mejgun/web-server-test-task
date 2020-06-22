{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where


import qualified Authors
import qualified Categories
import           Data.Default
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import qualified Network.Wai.Middleware.RequestLogger
                                               as RL

import           Lib
import qualified News
import qualified Tags
import qualified Users

main :: IO ()
main = do
  conf <- readConfig configFile
  createImagesDir (logger conf)
  putStrLn "Server started"
  l <- mkReqLogger conf
  run 8080 $ l $ application (connection conf) (logger conf)
 where
  mkReqLogger :: Config -> IO Middleware
  mkReqLogger conf = RL.mkRequestLogger $ def
    { RL.outputFormat = case loglevel conf of
                          LogDebug  -> RL.Detailed False
                          LogNormal -> RL.Apache RL.FromSocket
                          LogQuiet  -> RL.CustomOutputFormat (\_ _ _ _ -> "")
    , RL.autoFlush    = True
    , RL.destination  = RL.Handle (hnd conf)
    }

application :: MyApp
application c l r rd = case pathInfo r of
  ["user"    , "get"          ] -> norm Users.get
  ["user"    , "create"       ] -> norm Users.create
  ["user"    , "delete"       ] -> adm Users.delete
  ["user"    , "login"        ] -> norm Users.logIn
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
  norm x = normalHandler x c l r rd
  adm x = adminHandler x c l r rd
