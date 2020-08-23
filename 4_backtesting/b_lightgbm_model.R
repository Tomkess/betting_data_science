# ----- Load Prerequisities -----
source("C:/Users/Peter/Desktop/ds_projects/betting_data_science/4_backtesting/b_prerequisities.R")

# ----- create model data -----
dfit_lightgbm <- as(dfit %>% 
                      # dplyr::select(-match_result, -n_goals) %>%
                      as.matrix(), 
                    Class = "sparseMatrix")

# ----- Get Prediction for each League -----
dfit$lightgbm_pred <- 
  as.numeric(m_lightgbm$predict(object = m_lightgbm, 
                                data = dfit_lightgbm))

league_data <- master_data %>%
  select(HomeTeam, created_at, Div) %>%
  rename(team = HomeTeam, match_date = created_at, league = Div) %>%
  distinct() %>%
  
  rbind(., master_data %>%
          select(AwayTeam, created_at, Div) %>%
          rename(team = AwayTeam, match_date = created_at, league = Div) %>%
          distinct()) %>%
  
  distinct()

result_data <- 
  get_result_data(dfit_input = test_woe %>% 
                    mutate(lightgbm_pred = dfit$lightgbm_pred) %>%
                    left_join(., league_data), 
                  i_c_basic = colnames_basic, 
                  i_c_input = c(colnames_basic, "lightgbm_pred"), 
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

save(backtesting_output, file = "4_backtesting/db_temp/b_lightgbm_model.RData")
