select
	distinct fullname,
	sport_league,
	dateclosed,
	sport_category,
	eventtypedescription,
	type
from
	t_tipsport_bookmaker
where
	sport_category like '%Fotbal - mu%';
