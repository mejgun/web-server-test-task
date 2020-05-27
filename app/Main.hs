{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Network.Wai
import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai.Middleware.RequestLogger
                                                ( logStdout )
import qualified Data.Text                     as T

import           PG
import           Types
import           Users

application :: MyApp
application conn req respond = case pathInfo req of
  ["getusers"]       -> getUsers "0" conn req respond
  ["getusers", page] -> getUsers (T.unpack page) conn req respond
  ["createuser"]     -> createUser conn req respond
  ["deleteuser"]     -> deleteUser conn req respond
  ["loginuser" ]     -> loginUser conn req respond
  _                  -> respond responseERR


main :: IO ()
main = do
  conn <- pgconnect
  run 8080 $ logStdout $ application conn


