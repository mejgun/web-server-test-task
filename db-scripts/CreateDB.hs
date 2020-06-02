{-# LANGUAGE OverloadedStrings #-}

import           PG

-- v1 users table created
-- v2 authors table created
-- v3 categories table created
-- v4 news, news_photos, news_tags tables created
-- v5 users.photo set unique

main :: IO ()
main = do
  conn <- pgconnect
  mapM_
    (\x -> print =<< execute_ conn x)
    [ "create table if not exists users (id serial primary key, name varchar(255) not null, lastname varchar(255) not null, photo varchar(999), admin bool not null default false, token char(32) unique not null, login varchar(100) not null unique, password char(32) not null);"
    , "create table if not exists authors (id serial primary key, user_id integer not null unique references users(id) on delete cascade, descr text);"
    , "create table if not exists categories (id serial primary key, name text not null, parent integer references categories(id) on delete cascade);"
    , "create table if not exists tags (id serial primary key, name text not null unique);"
    , "create table if not exists news (id serial primary key, name varchar(999) not null, date date not null, author_id integer not null references authors(id) on delete restrict, category_id integer not null references categories(id) on delete restrict, text text not null, main_photo varchar(999), published bool not null default false);"
    , "create table if not exists news_photos (id serial primary key, news_id integer not null references news(id) on delete restrict, photo varchar(999) unique not null);"
    , "create table if not exists news_tags (id serial primary key, tag_id integer not null references tags(id) on delete cascade, news_id integer not null references news(id) on delete cascade, constraint unique_news_tag unique (tag_id,news_id));"
    , "do $$ begin if not exists (select constraint_name as a from information_schema.constraint_column_usage where constraint_name = 'users_photo_key') then alter table if exists users add constraint users_photo_key unique (photo); end if; end; $$;"
    ]
