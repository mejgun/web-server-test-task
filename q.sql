drop function getnews;
create function 
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