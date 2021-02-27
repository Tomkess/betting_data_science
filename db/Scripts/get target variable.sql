select
	distinct season,
	league,
	team,
	match_id,
	created_at,
	n_goals
from
	v_match_stats;
