{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Data.Text                     as T
                                                ( Text )
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )

import           Authors
import           PG
import           Types
import           Users

main :: IO ()
main = do
  conn <- pgconnect
  putStrLn "Server started"
  run 8080 $ logStdout $ application conn

application :: MyApp
application c r rd = case pathInfo r of
  [x] -> (route x) c r rd
  _   -> rd responseERR

route :: T.Text -> MyApp
route "getusers"     = getUsers
route "createuser"   = createUser
route "deleteuser"   = deleteUser
route "loginuser"    = loginUser
route "makeauthor"   = makeAuthor
route "editauthor"   = editAuthor
route "deleteauthor" = deleteAuthor
route "getauthors"   = getAuthors
route _              = \_ _ rd -> rd responseERR
