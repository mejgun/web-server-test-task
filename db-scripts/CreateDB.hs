{-# LANGUAGE OverloadedStrings #-}

import           PG

main :: IO ()
main = do
  conn <- pgconnect
  print =<< execute_
    conn
    "create table if not exists users (id serial primary key, name varchar(255) not null, lastname varchar(255) not null, photo varchar(999), admin bool not null default false, token char(32) not null);"


-- insert into users (name,lastname,token) values ('test','testlastname',md5(random()::text));
