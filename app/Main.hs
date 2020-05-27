{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Data.Text                     as T
import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )

import           Authors
import           PG
import           Types
import           Users

application :: MyApp
application c r rd = case pathInfo r of
  ["getusers"]       -> getUsers "0" c r rd
  ["getusers", page] -> getUsers (T.unpack page) c r rd
  ["createuser"]     -> createUser c r rd
  ["deleteuser"]     -> deleteUser c r rd
  ["loginuser" ]     -> loginUser c r rd
  ["makeauthor"]     -> makeAuthor c r rd
  ["editauthor"]     -> editAuthor c r rd
  _                  -> rd responseERR


main :: IO ()
main = do
  conn <- pgconnect
  putStrLn "Server started"
  run 8080 $ logStdout $ application conn


