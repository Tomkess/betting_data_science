drop database if exists betting_ds;

create database betting_ds with owner = postgres encoding = 'UTF8' LC_COLLATE = 'Czech_Czech Republic.1250' LC_CTYPE = 'Czech_Czech Republic.1250' tablespace = pg_default connection
limit = -1;
