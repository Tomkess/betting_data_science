library(goalmodel)

t_match_stats <- pin_get("t_match_stats") %>% 
  rowwise() %>% 
  dplyr::mutate(
    odds_25 = dplyr::coalesce(`b365.2.5`, `p.2.5`, `gb.2.5`),
    odds_251 = dplyr::coalesce(`b365.2.5.1`, `p.2.5.1`, `gb.2.5.1`),
    n_goals = fthg + ftag) %>% 
  dplyr::filter(is_home == 1) %>% 
  dplyr::select(season, league, team, created_at, match_id, odds_25, odds_251, n_goals) %>% 
  
  rename(home_team = team) %>% 
  as.data.frame() %>% 
  
  left_join(., pin_get("t_match_stats") %>% 
              rowwise() %>% 
              dplyr::filter(is_home == 0) %>% 
              dplyr::select(season, league, team, created_at, match_id) %>% 
              rename(away_team = team) %>% 
              as.data.frame()) %>% 
  as.data.frame() %>% 
  
  dplyr::filter(!(is.na(odds_25)) & !(is.na(odds_251))) %>% 
  dplyr::filter(season %in% c("1819", "1920", "2021")) %>% 
  as.data.frame()

results_data <-
  
  pin_get("modelling_data") %>% 
  dplyr::filter(is_home == 0) %>%
  rename(awayteam = team,
         at_goals = n_goals) %>% 
  dplyr::select(created_at, league, season, awayteam, match_id, at_goals) %>% 
  distinct() %>% 
  as.data.frame() %>% 
  
  left_join(.,   pin_get("modelling_data") %>% 
              dplyr::filter(is_home == 1) %>%
              rename(hometeam = team,
                     ht_goals = n_goals) %>% 
              dplyr::select(created_at, league, season, hometeam, match_id, ht_goals) %>% 
              distinct() %>% 
              as.data.frame()) %>% 
  na.omit() %>% 
  as.data.frame()

eval_df <- t_match_stats %>% 
  dplyr::group_by(season, league, created_at) %>% 
  nest() %>% 
  
  crossing(data.frame(hist_cat = c(1, 2, 3)))

eval_df$train_df <- list(NA)
eval_df$model <- list(NA)
eval_df$pred_df <- list(NA)

# subset data for training
for(i in 1:nrow(eval_df)){
  
  eval_df$train_df[[i]] <- 
    results_data %>% 
    dplyr::filter(league %in% eval_df$league[i] &
                    created_at < eval_df$created_at[i] &
                    created_at >= eval_df$created_at[i] - 360 * eval_df$hist_cat[i]) %>% 
    as.data.frame()
}

# model
for(i in 1:nrow(eval_df)){
  # for(i in 1:10){
  
  tmp_df <- eval_df$train_df[[i]]
  tmp_df$weights_dc <- weights_dc(tmp_df$created_at, xi = 0.0019)
  
  result_model <- tryCatch({
    goalmodel(goals1 = tmp_df$ht_goals, goals2 = tmp_df$at_goals,
              team1 = tmp_df$hometeam, team2 = tmp_df$awayteam,
              rs = TRUE, weights = tmp_df$weights_dc, dc = T)  
  }, error = function(error_condition) {
    list(model_status = "FAIL")
  })
  
  eval_df$model[[i]] <- result_model
  print(i)
}

# predict from model
for(i in 1:nrow(eval_df)){
  # for(i in 1:10){
  
  # i <- 1
  tmp_df <- eval_df$data[[i]]
  tmp_model <- eval_df$model[[i]]
  
  tmp_list <- list()
  count <- 1
  for(j in 1:nrow(tmp_df)){
    # j <- 3
    
    result <- tryCatch({
      predict_expg(tmp_model, 
                   team1 = tmp_df$home_team[j], 
                   team2 = tmp_df$away_team[j], 
                   return_df = TRUE)
    }, error = function(error_condition) {
      data.frame(team1 = tmp_df$home_team[j],
                 team2 = tmp_df$away_team[j],
                 expg1 = 0,
                 expg2 = 0,
                 pred_status = "FAIL")
    })
    
    if (!("pred_status" %in% names(result))){
      tmp_res <- predict_expg(tmp_model, 
                              team1 = tmp_df$home_team[j], 
                              team2 = tmp_df$away_team[j], 
                              return_df = TRUE) %>% as.data.frame()
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
      
      tmp_list[[count]] <- tmp_res
      count <- count + 1
    }
  }
  
  eval_df$pred_df[[i]] <- tmp_list %>% bind_rows() %>% as.data.frame()
  print(i)
}

pred_eval_df <- 
  eval_df %>% 
  dplyr::select(season, league, created_at, hist_cat, data) %>% 
  unnest(c(data)) %>% 
  distinct() %>% 
  
  left_join(., 
            eval_df %>% 
              dplyr::select(season, league, created_at, hist_cat, pred_df) %>% 
              unnest(c(pred_df)) %>% 
              distinct()) %>% 
  
  na.omit() %>% 
  as.data.frame() %>% 
  
  rbind(., pred_eval_df %>% 
          dplyr::group_by(season, league, created_at, match_id, odds_25, odds_251, n_goals, home_team, away_team) %>% 
          dplyr::summarise_at(., vars(prob_25, prob_251), mean) %>% 
          as.data.frame() %>% 
          
          dplyr::mutate(hist_cat = 99,
                        pred_res = dplyr::if_else(prob_25 > prob_251, "Under 2.5", "Over 2.5")) %>% 
          as.data.frame()) %>% 
  as.data.frame() %>% 
  
  dplyr::mutate(
    kelly_prob = 
      dplyr::if_else(prob_25 > prob_251, 
                     (((prob_25 * odds_25) - 1)/(odds_25 - 1)), 
                     (((prob_251 * odds_251) - 1)/(odds_251 - 1))),
    result = dplyr::if_else(n_goals > 2.5, "Over 2.5", "Under 2.5")) %>%
  
  dplyr::mutate(is_bet = dplyr::if_else(0.5 * kelly_prob > 0.1, 1, 0),
                bet_stake = is_bet * kelly_prob * 3000,
                win_loss = ifelse(result == pred_res, 1, -1),
                payout = ifelse(prob_25 > prob_251, odds_25, odds_251),
                wealth = ifelse(win_loss == 1, bet_stake * (payout - 1), -bet_stake)) %>% 
  as.data.frame()

pred_eval_df %>% 
  group_by(season, league, hist_cat) %>% 
  summarise(n_bet = sum(is_bet),
            total_profit = sum(wealth),
            roi = sum(wealth)/(sum(bet_stake)),
            placed_bet = sum(bet_stake),
            ratio_bets = sum(is_bet)/n(),
            n_matches = n()) %>% 
  View()

pred_eval_df %>% 
  group_by(season, hist_cat) %>% 
  summarise(n_bet = sum(is_bet),
            total_profit = sum(wealth),
            roi = sum(wealth)/(sum(bet_stake)),
            placed_bet = sum(bet_stake),
            ratio_bets = sum(is_bet)/n(),
            n_matches = n()) %>% 
  View()
