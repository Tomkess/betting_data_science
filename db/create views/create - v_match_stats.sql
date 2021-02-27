CREATE OR REPLACE VIEW v_match_stats as (
select
	season,
	league,
	created_at,
	team,
	is_home,
	match_id,
	match_results,
	total_goals,
	n_goals,
	n_shots,
	n_shots_ontarget,
	n_fauls,
	n_corners,
	n_yellow_cards,
	n_red_cards,
	r_shots_goals,
	r_st_goals,
	r_fauls_goals,
	r_corners_goals,
	r_yellow_goals,
	r_red_goals,
	r_team_odds,
	r_draw_odds,
	sum(match_results_n) over(partition by team,
	league,
	is_home
order by
	created_at asc) as o_strength_ah,
	sum(match_results_n) over(partition by team,
	league
order by
	created_at asc) as o_strength,
	sum(match_results_n) over(partition by team,
	league,
	season
order by
	created_at asc) as o_strength_season,
	sum(match_results_n) over(partition by team,
	league,
	season,
	is_home
order by
	created_at asc) as o_strength_season_ah
from
	(
	select
		trim(both season) as season,
		trim(both league) as league,
		created_at,
		trim(both team) as team,
		is_home,
		match_id,
		fthg + ftag as total_goals,
		case
			when ftr = 'H'
			and is_home = 1 then 1
			when ftr = 'A'
			and is_home = 0 then 1
			when ftr = 'A'
			and is_home = 1 then 0
			when ftr = 'H'
			and is_home = 0 then 0
			else -1
		end as match_results,
		case
			when ftr = 'H'
			and is_home = 1 then 3
			when ftr = 'A'
			and is_home = 0 then 3
			when ftr = 'A'
			and is_home = 1 then 0
			when ftr = 'H'
			and is_home = 0 then 0
			else 1
		end as match_results_n,
		case
			when is_home = 1 then fthg
			else ftag
		end as n_goals,
		case
			when is_home = 1 then hs
			else "as"
		end as n_shots,
		case
			when is_home = 1 then hst
			else ast
		end as n_shots_ontarget,
		case
			when is_home = 1 then hf
			else af
		end as n_fauls,
		case
			when is_home = 1 then hc
			else ac
		end as n_corners,
		case
			when is_home = 1 then hy
			else ay
		end as n_yellow_cards,
		case
			when is_home = 1 then hr
			else ar
		end as n_red_cards,
		case
			when is_home = 1 then hs /(fthg + 1)
			else "as" /(ftag + 1)
		end as r_shots_goals,
		case
			when is_home = 1 then hst /(fthg + 1)
			else ast /(ftag + 1)
		end as r_st_goals,
		case
			when is_home = 1 then hf /(fthg + 1)
			else af /(ftag + 1)
		end as r_fauls_goals,
		case
			when is_home = 1 then hc /(fthg + 1)
			else ac /(ftag + 1)
		end as r_corners_goals,
		case
			when is_home = 1 then hy /(fthg + 1)
			else ay /(ftag + 1)
		end as r_yellow_goals,
		case
			when is_home = 1 then hr /(fthg + 1)
			else ar /(ftag + 1)
		end as r_red_goals,
		case
			when is_home = 1 then coalesce(b365h, bwh, iwh, psh, whh, vch)
			else coalesce(b365a, bwa, iwa, psa, wha, vca)
		end as r_team_odds,
		coalesce(b365d, bwd, iwd, psd, whd, vcd) as r_draw_odds
	from
		t_match_stats) as m_data);
