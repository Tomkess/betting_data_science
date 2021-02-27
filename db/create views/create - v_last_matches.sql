create or replace
view v_last_matches as (
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 5 preceding and current row) as last_n,
	'last_5' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 10 preceding and current row) as last_n,
	'last_10' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 15 preceding and current row) as last_n,
	'last_15' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 20 preceding and current row) as last_n,
	'last_20' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 25 preceding and current row) as last_n,
	'last_25' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 30 preceding and current row) as last_n,
	'last_30' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 35 preceding and current row) as last_n,
	'last_35' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 40 preceding and current row) as last_n,
	'last_40' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 45 preceding and current row) as last_n,
	'last_45' as hist_category
from
	t_match_stats
union all
select
	distinct trim(both season) as season,
	trim(both league) as league,
	created_at,
	trim(both team) as team,
	is_home,
	min(created_at) over(partition by team,
	is_home
order by
	created_at rows between 50 preceding and current row) as last_n,
	'last_50' as hist_category
from
	t_match_stats );
