drop table if exists t_match_calendar cascade;
create table t_match_calendar ( match_id serial primary key,
created_at date not null,
league varchar(50) not null,
season varchar(50) not null,
home_t varchar(50) not null,
away_t varchar(50) not null,
file_link varchar(250) not null);
