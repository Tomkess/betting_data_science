select
	distinct trim(both tipsport_league) as tipsport_league,
	trim(both results_league) as results_league
from
	(
	select
		*,
		max(created_at) over(partition by tipsport_league,
		results_league) as last_update
	from
		t_mapping_league) as m_data
where
	created_at = last_update;
