drop table if exists t_tipsport_bookmaker;

create table if not exists t_tipsport_bookmaker ( eventtype varchar(250) not null,
eventtypedescription varchar(250) not null,
gamepart varchar(250) not null,
eventid varchar(250) not null,
eventname varchar(250) not null,
opportunityid varchar(250) not null,
number varchar(250) not null,
type varchar(250) not null,
fullname varchar(250) not null,
rate float not null,
dateclosed timestamp without time zone not null,
matchid varchar(250) not null,
namefull varchar(250) not null,
url varchar(250) not null,
urlold varchar(250) not null,
livematchid varchar(250),
competition_name varchar(250) not null,
annualname varchar(250) not null,
sport_league varchar(250) not null,
sport_url varchar(250) not null,
sport_urlold varchar(250) not null,
sport_id varchar(250) not null,
sport_category varchar(250) not null,
sport varchar(250) not null,
created_at timestamp without time zone not null);