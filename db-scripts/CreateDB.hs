{-# LANGUAGE OverloadedStrings #-}

import           PG

-- v1 users table created

main :: IO ()
main = do
  conn <- pgconnect
  mapM_
    (\x -> print =<< execute_ conn x)
    [ "create table if not exists users (id serial primary key, name varchar(255) not null, lastname varchar(255) not null, photo varchar(999), admin bool not null default false, token char(32) unique not null, login varchar(100) not null unique, password char(32) not null);"
    ]
