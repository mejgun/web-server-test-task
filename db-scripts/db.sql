-- v1 users table created
-- v2 authors table created
-- v3 categories table created
-- v4 news, news_photos, news_tags tables created
-- v5 users.photo set unique
-- v6 news_comments table created
-- v7 getnews function add

create table if not exists users (
 id serial primary key,
 name varchar(255) not null,
 lastname varchar(255) not null,
 photo varchar(999),
 admin bool not null default false,
 token char(32) unique not null,
 login varchar(100) not null unique,
 password char(32) not null
);

create table if not exists authors (
 id serial primary key,
 user_id integer not null unique references users(id) on delete cascade,
 descr text
);

create table if not exists categories (
 id serial primary key,
 name text not null,
 parent integer references categories(id) on delete cascade
);

create table if not exists tags (id serial primary key, name text not null unique);

create table if not exists news (
 id serial primary key,
 name varchar(999) not null,
 date date not null,
 author_id integer not null references authors(id) on delete restrict,
 category_id integer not null references categories(id) on delete restrict,
 text text not null,
 main_photo varchar(999),
 published bool not null default false
);

create table if not exists news_photos (
 id serial primary key,
 news_id integer not null references news(id) on delete restrict,
 photo varchar(999) unique not null
);

create table if not exists news_tags (
 id serial primary key,
 tag_id integer not null references tags(id) on delete cascade,
 news_id integer not null references news(id) on delete cascade,
 constraint unique_news_tag unique (tag_id, news_id)
);

do $$
begin
 if not exists (
  select constraint_name as a from information_schema.constraint_column_usage
  where constraint_name = 'users_photo_key')
 then
  alter table if exists users add constraint users_photo_key unique (photo);
 end if;
end;
$$;

create table if not exists news_comments (
 id serial primary key,
 news_id integer not null references news(id) on delete cascade,
 user_id integer not null references users(id) on delete cascade,
 text text not null
);

drop function if exists getNews;
create or replace function getnews(
 created_at date,
 created_before date,
 created_after date,
 author_contains text,
 name_contains text,
 text_contains text,
 anything_contains text,
 cat_id int, 
 tags_all int[],
 tags_any int[],
 sortby text,
 offs int,
 lim int)
returns table(
 id int,
 date text,
 name varchar,
 text text,
 main_photo varchar,
 author_name varchar,
 author_lastname varchar,
 category_id int,
 photos varchar[],
 tags text[],
 photo_count bigint) as $$
declare
 at text  := '';
 bf text  := '';
 aft text := '';
 ac text  := '';
 nc text  := '';
 tc text  := '';
 anc text := '';
 cid text := '';
 allt text := '';
 anyt text := '';
 srt text  := '';
begin
 if (created_at is not null) then at = concat('date=''', created_at, ''''); end if;
 if (created_before is not null) then bf = concat(' and date<''', created_before, ''''); end if;
 if (created_after is not null) then aft = concat(' and date>''', created_after, ''''); end if;
 if (author_contains is not null) then ac = concat(' and (u.name ilike ''%',author_contains,'%'' or u.lastname ilike ''%',author_contains,'%'')'); end if;
 if (name_contains is not null) then nc = concat(' and n.name ilike ''%', name_contains, '%'''); end if;
 if (text_contains is not null) then tc = concat(' and n.text ilike ''%', text_contains, '%'''); end if;
 if (anything_contains is not null) then anc = concat(
     ' and (and n.name ilike ''%', anything_contains, '%''',
     ' or n.text ilike ''%', anything_contains, '%''',
     ' or u.name ilike ''%',anything_contains,'%''',
     ' or u.lastname ilike ''%',anything_contains,'%''',
     ' or t.name ilike ''%',anything_contains,'%'')'
 ); end if;
 if (cat_id is not null) then cid = concat(' and c.id=',cat_id); end if;
 if (tags_all is not null) then allt = concat(' and ''',tags_all,''' = array(select tag_id from news_tags where news_id=n.id order by tag_id asc)::int[]'); end if;
 if (tags_any is not null) then anyt = concat(' and ''',tags_any,''' && array(select tag_id from news_tags where news_id=n.id order by tag_id asc)::int[]'); end if;
 srt := case 
  when sortby = 'author' then ' u.name asc, u.lastname asc, n.id asc'
  when sortby = 'category' then ' c.name asc, n.date asc, n.id asc'
  when sortby = 'photos' then ' photo_count desc, n.id asc'
  else ' n.date asc, n.id asc'
 end;
 return query execute 
  'select 
    n.id,
    n.date::text,
    n.name,
    n.text,
    n.main_photo,
    u.name,
    u.lastname,
    c.id,    
    array_agg(np.photo),
    array_agg(t.name),
    ((select (case when main_photo is null then 0 else 1 end) from news where id=n.id)+count(np.id)) as photo_count
   from news as n
   left join authors as a on n.author_id=a.id
   left join users as u on a.user_id=u.id
   left join categories as c on c.id=n.category_id
   left join news_tags as nt on nt.news_id=n.id
   left join tags as t on t.id=nt.tag_id
   left join news_photos as np on np.news_id=n.id
   where n.published=true ' || at || bf || aft || ac || nc || tc || anc || cid || allt || anyt || '
   group by n.id, u.name,u.lastname,c.id
   order by ' || srt || '
   offset ' || offs || ' limit ' || lim || ';';
end;
$$ language plpgsql;