{-# LANGUAGE OverloadedStrings #-}

import           PG

-- v1 users table created
-- v2 added login&password columns

main :: IO ()
main = do
  conn <- pgconnect
  mapM_
    (\x -> print =<< execute_ conn x)
    [ "create table if not exists users (id serial primary key, name varchar(255) not null, lastname varchar(255) not null, photo varchar(999), admin bool not null default false, token char(32) unique not null);"
    , "alter table users add column if not exists login varchar(100) not null unique;"
    , "alter table users add column if not exists password char(32) not null;"
    ]
