results_data <-
  pin_get("v_match_stats", "local") %>% 
  as.data.frame() %>% 
  distinct() %>%
  
  dplyr::select(season, league, created_at, team, is_home, 
                match_id, total_goals, n_goals) %>% 
  as.data.frame() %>%
  
  left_join(., pin_get("t_mapping_team", "local") %>% 
              as.data.frame(),
            by = c("team" = "results_team",
                   "league" = "results_league")) %>% 
  as.data.frame() %>% 
  
  dplyr::select(-team, -league) %>% 
  distinct() %>% 
  as.data.frame() %>% 
  
  dplyr::filter(is_home == 1) %>% 
  rename(home_team = tipsport_team,
         h_goals = n_goals) %>%
  dplyr::select(-is_home) %>% 
  
  left_join(., pin_get("v_match_stats", "local") %>% 
              as.data.frame() %>% 
              distinct() %>%
              
              dplyr::select(season, league, created_at, team, is_home, 
                            match_id, total_goals, n_goals) %>% 
              as.data.frame() %>%
              
              left_join(., pin_get("t_mapping_team", "local") %>% 
                          as.data.frame(),
                        by = c("team" = "results_team",
                               "league" = "results_league")) %>% 
              as.data.frame() %>% 
              
              dplyr::select(-team, -league) %>% 
              distinct() %>% 
              as.data.frame() %>% 
              
              dplyr::filter(is_home == 0) %>% 
              rename(away_team = tipsport_team,
                     a_goals = n_goals) %>%
              dplyr::select(-is_home, -total_goals) %>% 
              as.data.frame()) %>% 
  
  na.omit() %>% 
  dplyr::select(-match_id) %>% 
  as.data.frame() %>% 
  pin("results_data", "local")
