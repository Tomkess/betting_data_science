create or replace view v_upcoming_matches as (
select
	distinct created_at,
	dateclosed,
	fullname,
	sport_league,
	split_part(competition_name, '-', 1) t_home_team,
	split_part(competition_name, '-', 2) t_away_team,
	rate
from
	t_tipsport_bookmaker
where
	sport = 'Fotbal'
	and sport_category like '%mu%'
	and type in ('u',
	'o')
	and eventtype = '-91'
	and gamepart = '1/1'
	and fullname not like '%(%'
	and sport_league in (
	select
		distinct tipsport_league
	from
		t_mapping_league)
	and dateclosed > now());
