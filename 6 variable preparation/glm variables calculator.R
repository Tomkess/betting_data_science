
modelling_data <-
  pin_get("v_last_matches", "local") %>%
  
  dplyr::filter(hist_category %in% c("last_10", "last_20", "last_30")) %>%
  
  
  # - get data
  dplyr::group_by(league, team, is_home) %>%
  nest() %>%
  rename(h_data = data) %>%
  
  dplyr::left_join(., pin_get("v_match_stats", "local")) %>%
  dplyr::group_by(league, team, is_home) %>%
  nest(data = !c(h_data, league, team, is_home)) %>%
  dplyr::rename(data = h_data,
                match_data = data) %>%
  
  dplyr::group_by(team, is_home) %>%
  dplyr::mutate(
    hist_data = 
      map2(data, match_data, 
           function(h_data, m_data) {
             
             
             h_data %>%
               dplyr::mutate(days_diff = created_at - last_n) %>%
               map_df(., rep, .$days_diff) %>%
               dplyr::group_by(created_at, last_n) %>%
               dplyr::mutate(between_date = last_n + c(1:n()) - 1) %>%
               as.data.frame() %>%
               
               left_join(., m_data %>% 
                           rename(match_date = created_at), 
                         by = c("between_date" = "match_date", 
                                "season" = "season")) %>%
               dplyr::filter(!(is.na(match_id))) %>%
               
               dplyr::group_by(season, created_at, last_n, hist_category) %>%
               dplyr::summarise(
                 match_results = mean_x(match_results),
                 avg_total_goals = mean_x(total_goals),
                 n_goals = mean_x(n_goals),
                 n_shots = mean_x(n_shots),
                 n_shots_ontarget = mean_x(n_shots_ontarget),
                 n_fauls = mean_x(n_fauls),
                 n_corners = mean_x(n_corners),
                 n_yellow_cards = mean_x(n_yellow_cards),
                 n_red_cards = mean_x(n_red_cards),
                 r_shots_goals = mean_x(r_shots_goals),
                 r_st_goals = mean_x(r_st_goals),
                 r_fauls_goals = mean_x(r_fauls_goals),
                 r_corners_goals = mean_x(r_corners_goals),
                 r_yellow_goals = mean_x(r_yellow_goals),
                 r_red_goals = mean_x(r_red_goals),
                 r_team_odds = mean_x(r_team_odds),
                 r_draw_odds = mean_x(r_draw_odds),
                 r_ah_advantage = mean_x(o_strength_ah)/(mean_x(o_strength) + 1),
                 r_ah_advantage_season = mean_x(o_strength_ah)/(mean_x(o_strength_season) + 1),
                 r_season_strength = mean_x(o_strength_season)/(mean_x(o_strength) + 1),
                 r_season_strength_ah = mean_x(o_strength_season_ah)/(mean_x(o_strength_ah) + 1)
               ) %>%
               as.data.frame()
             
           })) %>%
  dplyr::select(-data, -match_data) %>%
  unnest(c(hist_data)) %>%
  as.data.frame() %>%
  
  gather(., var_name, var_value, 
         -c(league, season, team, is_home, 
            created_at, last_n, hist_category)) %>%
  dplyr::mutate(var_name_n = paste(var_name, hist_category, sep = "__")) %>%
  
  dplyr::select(-hist_category, -var_name, -last_n) %>%
  distinct() %>%
  as.data.frame() %>% 
  
  # duplicated teams - summarization
  dplyr::group_by(league, team, is_home, season, created_at, var_name_n) %>% 
  dplyr::summarise_at(., vars(var_value), mean) %>% 
  as.data.frame() %>% 
  
  # - removing too historical seasons
  dplyr::filter(!(season %in% c("9495", "9596", "9697", "9798", "9899", "9900",
                                "0001", "0102", "0203", "0304", "0405", "0506",
                                "0607", "0708", "0809", "0910", "1011", "1112",
                                "1213", "1314", "1415"))) %>%
  
  # spread to columns
  tidyr::spread(., var_name_n, var_value) %>%
  as.data.frame() %>%
  
  left_join(., pin_get("target_vars", "local") %>%
              mutate(n_goals_cat = ifelse(n_goals > 2.5,
                                          "Over 2.5", "Under 2.5")) %>%
              distinct() %>% 
              as.data.frame()) %>%
  
  distinct() %>%
  as.data.frame() %>%
  pin("modelling_data", "local")
