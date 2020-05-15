# ----- Load Prerequisities -----
source("C:/Users/Peter/Desktop/ds_projects/betting_data_science/4_backtesting/b_prerequisities.R")

# ----- Prepare xgboost Data -----
dfit_xgboost <- 
  xgb.DMatrix(data.matrix(dfit %>% 
                            select(-n_goals, -match_result, -team, 
                                   -league, -match_date) %>% 
                            as.data.frame()), 
              label = dfit$n_goals, 
              missing = NA)

# ----- Get Prediction -----
dfit$xgboost_pred <- predict(xgboost_model_output, dfit_xgboost)

# ----- Get Result Data -----
result_data <- 
  get_result_data(dfit_input = dfit, 
                  i_c_basic = colnames_basic, 
                  i_c_input = c(colnames_basic, "xgboost_pred"), 
                  i_round_data = round_data)

# ----- Create Backtesting Data -----
master_backtesting <- get_backtesting_data() %>% as.data.frame()

# ---- Run Opt using various payoff -----
backtesting_output <- 
  master_backtesting %>%
  
  # - nesting match data
  group_by(Div, season, round, HomeTeam, AwayTeam, 
           created_at, FTR, total_goals, goals_result) %>%
  nest() %>%
  rename(match_data = data) %>%
  
  # - create poisson distribution
  mutate(poisson_data = list(get_poisson(match_data))) %>%
  mutate(pred_prob = list(get_probs(poisson_data))) %>%
  
  dplyr::select(-poisson_data) %>%
  unnest(c(pred_prob, match_data)) %>%
  
  group_by(Div, season, round) %>%
  nest() %>%
  
  mutate(opt_data_g = list(get_opt_data(data))) %>%
  filter(season %in% unique(round_data$season)) %>%
  unnest(c(opt_data_g)) %>%
  select(-data) %>%
  gather(., "payoff_type", "payoff", 
         -Div, -season, -round, -HomeTeam, -AwayTeam, -goals_result, 
         -my_pred_result, -my_pred) %>%
  
  group_by(Div, season, round, payoff_type) %>%
  nest()

backtesting_output <- backtesting_output %>%
  mutate(res_opt_data = list(NA))
for(i in 1:nrow(backtesting_output)){
  # i <- 292
  
  i_data <- backtesting_output$data[[i]]
  
  # - my prediction
  i_prob <- as.numeric(i_data %>% pull(my_pred))
  i_payoff <- as.numeric(i_data %>% pull(payoff))
  
  if(sum(is.na(i_payoff)) < 1 & length(i_payoff) > 3){
    opt_bet <- 
      solnp(pars = c(rep(0.01, length(i_prob))),
            fun = sharpe_fct, 
            eqfun = allocation_budget, 
            eqB = b_fraction,
            LB = c(rep(0.0, length(i_prob))),
            UB = c(rep(1, length(i_prob))), 
            control = list(trace = 0))
    opt_fr <- opt_bet$pars
  }else{
    opt_fr <- rep(0, length(i_prob))
  }
  
  data_output <- i_data %>%
    as.data.frame() %>%
    mutate(o_fr = opt_fr)
  
  backtesting_output$res_opt_data[[i]] <- data_output
  
  # - clean workspace
  rm(i_data)
  rm(i_payoff)
  rm(i_prob)
  rm(opt_fr)
}

save(backtesting_output, 
     file = "4_backtesting/db_temp/b_xgboost_countmodel.RData")
