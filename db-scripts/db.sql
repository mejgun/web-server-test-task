-- v1 users table created
-- v2 authors table created
-- v3 categories table created
-- v4 news, news_photos, news_tags tables created
-- v5 users.photo set unique
-- v6 news_comments table created
-- v7 getnews function add

create table if not exists users (id serial primary key, name varchar(255) not null, lastname varchar(255) not null, photo varchar(999), admin bool not null default false, token char(32) unique not null, login varchar(100) not null unique, password char(32) not null);

create table if not exists authors (id serial primary key, user_id integer not null unique references users(id) on delete cascade, descr text);

create table if not exists categories (id serial primary key, name text not null, parent integer references categories(id) on delete cascade);

create table if not exists tags (id serial primary key, name text not null unique);

create table if not exists news (id serial primary key, name varchar(999) not null, date date not null, author_id integer not null references authors(id) on delete restrict, category_id integer not null references categories(id) on delete restrict, text text not null, main_photo varchar(999), published bool not null default false);

create table if not exists news_photos (id serial primary key, news_id integer not null references news(id) on delete restrict, photo varchar(999) unique not null);

create table if not exists news_tags (id serial primary key, tag_id integer not null references tags(id) on delete cascade, news_id integer not null references news(id) on delete cascade, constraint unique_news_tag unique (tag_id,news_id));

do $$ 
begin 
 if not exists (select constraint_name as a from information_schema.constraint_column_usage where constraint_name = 'users_photo_key') 
 then alter table if exists users add constraint users_photo_key unique (photo); end if; 
 end; 
$$;

create table if not exists news_comments (id serial primary key, news_id integer not null references news(id) on delete cascade, user_id integer not null references users(id) on delete cascade, text text not null);

create or replace function 
 getnews(created_at date, created_before date, created_after date,author_contains text)
 returns table(id int,name varchar,text text,author_name varchar,author_lastname varchar) as $$
declare 
 at  text := ' and true';
 bf  text := ' and true';
 aft text := ' and true';
 au  text := ' and true';
begin    
 if (created_at is not null) then at=concat('date=''',created_at,''''); end if;
 if (created_before is not null) then bf=concat(' and date<''',created_before,''''); end if;
 if (created_after is not null) then aft=concat(' and date>''',created_after,''''); end if;
 if (author_contains is not null) then au=concat(' and (u.name ilike ''%',author_contains,'%'' or u.lastname ilike ''%',author_contains,'%'')'); end if;
 return query execute 'select n.id,n.name,n.text,u.name,u.lastname from news as n, authors as a, users as u where n.author_id=a.id and a.user_id=u.id ' || at || bf || aft || au || ';';
end;
$$ language plpgsql;