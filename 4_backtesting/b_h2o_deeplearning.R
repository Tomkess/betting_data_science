# ----- Load Prerequisities -----
source("C:/Users/Peter/Desktop/ds_projects/betting_data_science/4_backtesting/b_prerequisities.R")

# ----- Get Prediction -----
dfit_h2o <- 
  
  # - select evaluation period
  dfit %>%
  
  # - prepare data using training data structure
  dplyr::select(-match_date, -match_result, -team, -league, -n_goals) %>%
  as.data.frame() %>%
  as.h2o()

dfit$h2o_pred <- 
  as.numeric(as.data.frame(h2o.predict(object = model_h2o,
                                       newdata = dfit_h2o))$predict)

result_data <- 
  get_result_data(dfit_input = dfit, 
                  i_c_basic = colnames_basic, 
                  i_c_input = c(colnames_basic, "h2o_pred"), 
                  i_round_data = round_data)
gc()

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

save(backtesting_output, file = "4_backtesting/db_temp/b_h2o_deeplearning.RData")
