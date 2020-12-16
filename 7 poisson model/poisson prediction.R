library(goalmodel)

has_history_df <-
  pin_get("results_data", "local") %>% 
  dplyr::select(tipsport_league, home_team) %>% 
  rename(team = home_team) %>% 
  distinct() %>% 
  
  rbind(., pin_get("results_data", "local") %>% 
          dplyr::select(tipsport_league, away_team) %>% 
          rename(team = away_team) %>% 
          distinct()) %>% 
  distinct() %>% 
  dplyr::mutate(has_history = 1) %>% 
  as.data.frame()

upcoming_matches <- 
  pin_get("v_upcoming_matches", "local") %>% 
  dplyr::filter(dateclosed >= Sys.time()) %>% 
  
  dplyr::filter(fullname %in% c("Více než 2.5", "Méně než 2.5")) %>% 
  dplyr::mutate(
    rate_type = dplyr::if_else(fullname == "Méně než 2.5", 
                               "odds_25", "odds_251")) %>% 
  dplyr::select(-fullname) %>% 
  tidyr::spread(., rate_type, rate) %>% 
  as.data.frame() %>% 
  
  left_join(., has_history_df, 
            by = c("t_home_team" = "team",
                   "sport_league" = "tipsport_league")) %>% 
  rename(home_history = has_history) %>% 
  
  left_join(., has_history_df, 
            by = c("t_away_team" = "team",
                   "sport_league" = "tipsport_league")) %>% 
  rename(away_history = has_history) %>% 
  
  # keep only matches with history
  na.omit() %>% 
  dplyr::select(-home_history, -away_history) %>% 
  
  # keep only today's matches and today's data
  dplyr::filter(as.Date(dateclosed) %in% Sys.Date() &
                  as.Date(created_at) %in% Sys.Date()) %>% 
  as.data.frame()

upcoming_matches$prob_25 <- NA
upcoming_matches$prob_251 <- NA
upcoming_matches$pred_res <- NA

for(i in 1:nrow(upcoming_matches)){
  
  # i <- 1
  
  # subset data
  tmp_data <- 
    pin_get("results_data", "local") %>% 
    dplyr::filter(tipsport_league %in% upcoming_matches$sport_league[i] &
                    created_at < as.Date(upcoming_matches$dateclosed[i]) &
                    created_at >= as.Date(upcoming_matches$dateclosed[i]) - 360 * 2)
  
  # fit model
  tmp_data$weights_dc <- weights_dc(tmp_data$created_at, xi = 0.0019)
  
  result_model <- tryCatch({
    goalmodel(goals1 = tmp_data$h_goals, goals2 = tmp_data$a_goals,
              team1 = tmp_data$home_team, team2 = tmp_data$away_team,
              rs = TRUE, weights = tmp_data$weights_dc, dc = T)  
  }, error = function(error_condition) {
    list(model_status = "FAIL")
  })
  
  # predict
  result <- tryCatch({
    predict_expg(result_model, 
                 team1 = upcoming_matches$t_home_team[i], 
                 team2 = upcoming_matches$t_away_team[i], 
                 return_df = TRUE)
  }, error = function(error_condition) {
    data.frame(team1 = upcoming_matches$t_home_team[i], 
               team2 = upcoming_matches$t_away_team[i], 
               expg1 = 0,
               expg2 = 0,
               pred_status = "FAIL")
  })
  
  if (!("pred_status" %in% names(result))){
    tmp_res <- 
      predict_expg(result_model, 
                   team1 = upcoming_matches$t_home_team[i], 
                   team2 = upcoming_matches$t_away_team[i], 
                   return_df = TRUE)
    rownames(tmp_res) <- c(1:nrow(tmp_res))
    names(tmp_res) <- c("home_team", "away_team", "lambda_ht", "lambda_at")
    
    tmp_res <- tmp_res %>% 
      crossing(data.frame(at_goals = c(0:10))) %>% 
      crossing(data.frame(ht_goals = c(0:10))) %>% 
      as.data.frame()
    
    tmp_res <- tmp_res %>% 
      mutate(prob = dpois(ht_goals, lambda_ht) * dpois(at_goals, lambda_at),
             tot_goals = ht_goals + at_goals) %>% 
      dplyr::filter(tot_goals <= 2.5) %>% 
      dplyr::group_by(home_team, away_team) %>% 
      dplyr::summarise(prob_25 = sum(prob),
                       prob_251 = 1 - sum(prob)) %>% 
      as.data.frame() %>% 
      
      mutate(pred_res = ifelse(prob_25 > prob_251, "Under 2.5", "Over 2.5")) %>% 
      as.data.frame()
    
    upcoming_matches$prob_251[i] <- tmp_res$prob_251[1]
    upcoming_matches$prob_25[i] <- tmp_res$prob_25[1]
    upcoming_matches$pred_res[i] <- tmp_res$pred_res[1]
  }else{
    upcoming_matches$prob_251[i] <- 0
    upcoming_matches$prob_25[i] <- 0
    upcoming_matches$pred_res[i] <- "Not Possible"
  }
  
  print(i)
  
}

# Kelly Criterion
upcoming_matches <- 
  upcoming_matches %>% 
  dplyr::mutate(
    kelly_prob = 
      ifelse(prob_25 > prob_251, 
             ((prob_25 * odds_25 - 1)/(odds_25 - 1)), 
             ((prob_251 * odds_251 - 1)/(odds_251 - 1)))) %>%
  
  dplyr::mutate(is_bet = dplyr::if_else(0.25 * kelly_prob > 0.1, 1, 0),
                bet_stake = is_bet * kelly_prob * 3000) %>% 
  as.data.frame()
