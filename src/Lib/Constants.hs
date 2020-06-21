{-# LANGUAGE OverloadedStrings #-}

module Lib.Constants where

usersPerPage :: Int
usersPerPage = 10

authorsPerPage :: Int
authorsPerPage = 5

categoriesPerPage :: Int
categoriesPerPage = 15

tagsPerPage :: Int
tagsPerPage = 20

commentsPerPage :: Int
commentsPerPage = 10

newsPerPage :: Int
newsPerPage = 10

imagesDir :: String
imagesDir = "images/"

ok :: String
ok = "ok"

configFile :: FilePath
configFile = "config.json"
