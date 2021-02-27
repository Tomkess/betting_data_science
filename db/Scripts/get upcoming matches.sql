select
	distinct created_at,
	dateclosed,
	fullname,
	sport_league,
	trim(both from t_home_team) as t_home_team,
	trim(both from t_away_team) as t_away_team,
	rate
from
	v_upcoming_matches;
