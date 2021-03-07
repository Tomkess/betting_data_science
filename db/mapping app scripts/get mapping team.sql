select
	distinct tipsport_league,
	results_league,
	results_team,
	tipsport_team
from
	(
	select
		distinct created_at,
		tipsport_league,
		results_league,
		results_team,
		tipsport_team,
		last_update
	from
		(
		select
			created_at,
			trim(both tipsport_league) as tipsport_league,
			trim(both results_league) as results_league,
			trim(both results_team) as results_team,
			trim(both tipsport_team) as tipsport_team,
			max(created_at) over(partition by tipsport_league,
			results_league,
			results_team,
			tipsport_team) as last_update
		from
			t_mapping_team) as m_data
	where
		created_at = last_update) actual_mapping;
