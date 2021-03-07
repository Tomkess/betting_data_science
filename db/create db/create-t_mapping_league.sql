drop table t_mapping_league cascade;
create table if not exists t_mapping_league ( created_at timestamp without time zone not null,
tipsport_league varchar(50) not null,
results_league varchar(50) not null);
