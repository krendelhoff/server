drop table if exists checkedout;
drop table if exists tools;
drop table if exists users;

create table users (
       user_id serial primary key,
       username text
       );

create table tools (
       tool_id serial primary key,
       name text,
       description text,
       lastTouched date,
       timesBorrowed integer
       );

create table checkedout (
       user_id integer,
       tool_id integer
       );

insert into users (username) values ('willkurt');

insert into tools (name,description,lastTouched,timesBorrowed)
values ('hammer','hits stuff','2017-01-01',0);

insert into tools (name,description,lastTouched,timesBorrowed)
values ('saw','cuts stuff','2017-01-01',0);
