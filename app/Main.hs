{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

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
  ["getusers"    ] -> f getUsers
  ["createuser"  ] -> f createUser
  ["deleteuser"  ] -> f deleteUser
  ["loginuser"   ] -> f loginUser
  ["makeauthor"  ] -> f makeAuthor
  ["editauthor"  ] -> f editAuthor
  ["deleteauthor"] -> f deleteAuthor
  ["getauthors"  ] -> f getAuthors
  _                -> rd responseERR
  where f x = rIfJsonBody x c r rd
