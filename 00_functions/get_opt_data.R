get_opt_data <- function(df_i){
  # data_temp <- df_i
  
  data_temp <- 
    df_i %>%
    as.data.frame() %>%
    rowwise() %>%
    
    # - get prediction
    mutate(my_pred = max(prob_25, prob_251),
           my_pred_result = 
             ifelse(prob_25 > prob_251, 
                    "Under 2.5", "Over 2.5")) %>%
    
    # - get payoff
    mutate(payoff_B365 = 
             ifelse(prob_25 > prob_251, B365.2.5, B365.2.5.1),
           payoff_P = 
             ifelse(prob_25 > prob_251, P.2.5, P.2.5.1),
           payoff_GB = 
             ifelse(prob_25 > prob_251, GB.2.5, GB.2.5.1),
           
           # - get avg payout
           payoff_avg = 
             ifelse(prob_25 > prob_251, 
                    mean(c(B365.2.5, P.2.5, GB.2.5), na.rm = T), 
                    mean(c(B365.2.5.1, P.2.5.1, GB.2.5.1), na.rm = T)),
           
           payoff_coalesce = 
             ifelse(prob_25 > prob_251, 
                    coalesce(B365.2.5, P.2.5, GB.2.5), 
                    coalesce(B365.2.5.1, P.2.5.1, GB.2.5.1))) %>%
    dplyr::select(HomeTeam, AwayTeam, goals_result, 
                  my_pred_result, my_pred, 
                  payoff_B365, payoff_P, payoff_GB, 
                  payoff_avg, payoff_coalesce)
  
  return(data_temp %>% as.data.frame())
}