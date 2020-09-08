get_backtesting_data <- function(){
  
  master_temp <- master_data %>%
    filter(created_at >= as.Date(eval_period)) %>%
    as.data.frame() %>%
    
    rowwise() %>%
    mutate(avg_odds_a = (B365A/6 + BWA/6 + IWA/6 + PSA/6 + WHA/6 + VCA/6),
           avg_odds_h = (B365H/6 + BWH/6 + IWH/6 + PSH/6 + WHH/6 + VCH/6),
           avg_odds_d = (B365D/6 + BWD/6 + IWD/6 + PSD/6 + WHD/6 + VCD/6),
           
           avg_odds_25 = (B365.2.5/3 + P.2.5/3 + GB.2.5/3),
           avg_odds_251 = (B365.2.5.1/3 + P.2.5.1/3 + GB.2.5.1/3),
           
           clsc_odds_25 = coalesce(B365.2.5, P.2.5, GB.2.5),
           clsc_odds_251 = coalesce(B365.2.5.1, P.2.5.1, GB.2.5.1),
           
           total_goals = FTAG + FTHG,
           goals_result = ifelse(FTAG + FTHG > 2.5, "Over 2.5", "Under 2.5")) %>%
    
    dplyr::select(Div, HomeTeam, AwayTeam, created_at, 
                  FTR, total_goals, goals_result,
                  
                  # - select odds columns A, H, D
                  avg_odds_a, B365A, BWA, IWA, PSA, WHA, VCA,
                  avg_odds_h, B365H, BWH, IWH, PSH, WHH, VCH,
                  avg_odds_d, B365D, BWD, IWD, PSD, WHD, VCD,
                  
                  # - select columns with 2.5 and 2.5.1 odds
                  avg_odds_25, B365.2.5, P.2.5, GB.2.5,
                  avg_odds_251, B365.2.5.1, P.2.5.1, GB.2.5.1)
  
  master_backtesting <- master_temp %>%
    as.data.frame() %>%
    distinct() %>%
    
    # - Join Away Prediction
    left_join(., result_data %>% 
                as.data.frame() %>%
                select(-model, -match_result, -n_goals) %>% 
                filter(is_home == 0) %>%   
                as.data.frame() %>%
                distinct(),
              by = c("Div" = "league", 
                     "created_at" = "match_date", 
                     "AwayTeam" = "team")) %>%
    dplyr::select(-is_home) %>%
    rename(pred_A = pred) %>%
    
    # - Join Home Prediction
    left_join(., result_data %>%
                as.data.frame() %>%
                dplyr::select(league, match_date, team, pred, is_home) %>% 
                filter(is_home == 1) %>%
                as.data.frame() %>%
                distinct(), 
              by = c("Div" = "league", 
                     "created_at" = "match_date", 
                     "HomeTeam" = "team")) %>%
    dplyr::select(-is_home) %>%
    rename(pred_H = pred) %>%
    
    as.data.frame() %>%
    distinct() %>%
    
    # - Subset only relevant leagues
    filter(Div %in% unique(result_data$league))
  
  return(master_backtesting)
}