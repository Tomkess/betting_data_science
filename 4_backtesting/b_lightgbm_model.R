# ---- Library Load ----
library(dplyr)
library(data.table)
library(lightgbm)
library(tidyr)
library(purrr)
library(Matrix)
library(Rsolnp)

# ----- set evaluation period -----
eval_period <- "2018-07-01"
b_fraction <- 0.2

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Light GBM Model -----
model_lightgbm <- readRDS.lgb.Booster("2_ml_pipelines/db_temp/5_lightgbm_model.rds")

# ----- Load Modelling Data -----
load("2_ml_pipelines/db_temp/modelling_data.RData")

# ----- Load Match Data -----
load("0_etl/db_temp/0_results_download.RData")

# - get modelling data
dfit <- 
  modelling_data %>%
  filter(match_date > eval_period) %>%
  distinct() %>%
  as.data.frame()

# ----- create model data -----
deval <- as(dfit %>% 
              dplyr::select(-match_result, -n_goals, -match_date, -league, -team) %>%
              as.matrix(), 
            Class = "sparseMatrix")

# ----- Specify Allocation Budget ----- 
allocation_budget <- function(fr){
  sum(fr)
}

# ----- Sharpe Function -----
sharpe_fct <- function(fr){
  
  mean_p <- matrix(i_prob * i_payoff - (1 - i_prob), nrow = 1)
  sd_p <- diag(i_prob * (1 - i_prob) * (i_payoff^2))
  i_fr <- t(matrix(fr, nrow = 1))
  
  return((-1) * 
           as.numeric((mean_p %*% i_fr)/sqrt(t(i_fr) %*% sd_p %*% t(t(i_fr)))))
}

# ----- Get Prediction for each League -----
dfit$lightgbm_pred <- 
  as.numeric(model_lightgbm$predict(object = model_lightgbm, 
                                    data = deval))

# ----- Preapre Results Data - from model prediction -----
colname_input <- 
  c("league", "match_date", "is_home", 
    "team", "match_result", "n_goals", 
    names(dfit)[stringr::str_detect(names(dfit), 
                                    "lightgbm_") == T])
colnames_basic <- c("league", "match_date", "is_home", 
                    "team", "match_result", "n_goals")

# ----- Determine the Round during the season -----
round_data <- 
  data.frame(year = c(min(lubridate::year(master_data$created_at)):max(lubridate::year(master_data$created_at))))
round_data <- round_data %>%
  mutate(lag_year = lag(year)) %>%
  na.omit() %>%
  rowwise() %>%
  mutate(season = paste(lag_year, year, sep = "/")) %>%
  
  mutate(from_date = as.Date(paste(lag_year, "-07", "-01", sep = "")),
         to_date = as.Date(paste(year, "-07", "-31", sep = ""))) %>%
  dplyr::select(-year, -lag_year)

result_data <- 
  dfit %>%
  
  dplyr::select(one_of(colname_input)) %>%
  gather("model", "pred", -all_of(colnames_basic)) %>%
  as.data.frame() %>%
  distinct() %>%
  
  rowwise() %>%
  mutate(season = list(round_data %>% 
                         filter(match_date >= from_date & 
                                  match_date <= to_date))) %>%
  
  unnest(c(season)) %>%
  as.data.frame() %>%
  dplyr::select(-from_date, -to_date) %>%
  
  group_by(league, season, team) %>%
  arrange(match_date) %>%
  mutate(round = row_number())

# ----- Prepare Matches with Bookmakers Odds -----
master_temp <- master_data %>%
  filter(created_at > as.Date(eval_period)) %>%
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

rm(round_data)
rm(colname_input)
rm(colnames_basic)
rm(model_lightgbm)
rm(dfit)
rm(deval)
rm(modelling_data)
gc()

# ----- Create Backtesting Data -----
master_backtesting <- master_temp %>%
  as.data.frame() %>%
  
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
  
  as.data.frame()

# ---- Run Opt using various payoff -----
backtesting_output <- 
  master_backtesting %>%
  as.data.frame() %>%
  
  # - evaluation period
  rowwise() %>%
  mutate(eval_period = 
           ifelse(created_at > as.Date(eval_period), "EVAL", "NOT EVAL")) %>%
  as.data.frame() %>%
  filter(eval_period %in% "EVAL") %>%
  
  # - nesting the data
  group_by(Div, season, round, HomeTeam, AwayTeam, 
           created_at, FTR, total_goals, goals_result) %>%
  nest() %>%
  rename(match_data = data) %>%
  
  # - create poisson distribution
  group_by(Div, season, round, HomeTeam, AwayTeam, 
           created_at, FTR, total_goals, goals_result) %>%
  mutate(poisson_data = 
           map(match_data, function(df_input) {
             
             # - df_input <- backtesting_output$data[[1]]
             prob_H <- unique(df_input %>% pull(pred_H))
             prob_A <- unique(df_input %>% pull(pred_A))
             
             vec_H <- ((prob_H^c(0:10))*exp(-prob_H))/factorial(c(0:10))
             vec_A <- ((prob_A^c(0:10))*exp(-prob_A))/factorial(c(0:10))
             
             goals_data <- t(t(vec_H)) %*% t(vec_A)
             
             return(as.data.frame(goals_data))
           })) %>%
  
  # - calculate probabilities
  mutate(pred_prob = 
           map(poisson_data, 
               function(df_input){
                 
                 mat_input <- as.matrix(df_input)
                 # mat_input <- as.matrix(backtesting_output$poisson_data[[1]])
                 
                 prob_25 <- 
                   mat_input[1,1] + mat_input[1,2] + mat_input[1,3] +
                   mat_input[2,1] + mat_input[2,2] +
                   mat_input[3,1]
                 prob_251 <- 1 - prob_25
                 
                 prob_h <- sum(sum(lower.tri(mat_input, diag = F) * mat_input))
                 prob_a <- sum(sum(upper.tri(mat_input, diag = F) * mat_input))
                 prob_d <- 1 - prob_h - prob_a
                 
                 return(data.frame("prob_25" = prob_25,
                                   "prob_251" = prob_251,
                                   "prob_h" = prob_h,
                                   "prob_a" = prob_a,
                                   "prob_d" = prob_d))
               })) %>%
  dplyr::select(-poisson_data) %>%
  unnest(c(pred_prob, match_data)) %>%
  
  group_by(Div, season, round) %>%
  nest() %>%
  
  group_by(Div, season, round) %>%
  mutate(opt_data_g = 
           map(data, function(df_i){
             # data_temp <- df_i
             
             data_temp <- 
               df_i %>%
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
             
             return(data_temp)
           })) %>%
  filter(!is.na(season))

backtesting_output <- 
  backtesting_output %>% 
  mutate(res_opt_data = list(NA))
for(i in 1:nrow(backtesting_output)){
  df_input <- backtesting_output$opt_data_g[[i]]
  
  if(nrow(df_input) > 2){
    # - my prediction
    i_prob <- as.numeric(df_input %>% pull(my_pred))
    l_par <- length(i_prob)
    
    # - set up payoff 1
    i_payoff <- df_input %>% pull(payoff_B365)
    if(sum(is.na(i_payoff)) < 1){
      opt_bet <- 
        solnp(pars = c(rep(0.01, l_par)),
              fun = sharpe_fct, 
              eqfun = allocation_budget, 
              eqB = b_fraction,
              LB = c(rep(0.0, l_par)),
              UB = c(rep(1, l_par)), control = list(trace = 0))
      opt_fr_B365 <- opt_bet$pars
    }else{
      opt_fr_B365 <- rep(0, l_par)
    }
    
    # - set up payoff 2
    i_payoff <- df_input %>% pull(payoff_P)
    if(sum(is.na(i_payoff)) < 1){
      opt_bet <- 
        solnp(pars = c(rep(0.01, l_par)),
              fun = sharpe_fct, 
              eqfun = allocation_budget, 
              eqB = b_fraction,
              LB = c(rep(0.0, l_par)),
              UB = c(rep(1, l_par)), control = list(trace = 0))
      opt_fr_P <- opt_bet$pars
    }else{
      opt_fr_P <- rep(0, l_par)
    }
    
    # - set up payoff 3
    i_payoff <- df_input %>% pull(payoff_GB)
    if(sum(is.na(i_payoff)) < 1){
      opt_bet <- 
        solnp(pars = c(rep(0.01, l_par)),
              fun = sharpe_fct, 
              eqfun = allocation_budget, 
              eqB = b_fraction,
              LB = c(rep(0.0, l_par)),
              UB = c(rep(1, l_par)), control = list(trace = 0))
      opt_fr_GB <- opt_bet$pars
    }else{
      opt_fr_GB <- rep(0, l_par)
    }
    
    # - set up payoff 4
    i_payoff <- df_input %>% pull(payoff_avg)
    if(sum(is.na(i_payoff)) < 1){
      opt_bet <- 
        solnp(pars = c(rep(0.01, l_par)),
              fun = sharpe_fct, 
              eqfun = allocation_budget, 
              eqB = b_fraction,
              LB = c(rep(0.0, l_par)),
              UB = c(rep(1, l_par)), control = list(trace = 0))
      opt_fr_avg <- opt_bet$pars
    }else{
      opt_fr_avg <- rep(0, l_par)
    }
    
    # - set up payoff 5
    i_payoff <- df_input %>% pull(payoff_coalesce)
    if(sum(is.na(i_payoff)) < 1){
      opt_bet <- 
        solnp(pars = c(rep(0.01, l_par)),
              fun = sharpe_fct, 
              eqfun = allocation_budget, 
              eqB = b_fraction,
              LB = c(rep(0.0, l_par)),
              UB = c(rep(1, l_par)), control = list(trace = 0))
      opt_fr_coalesce <- opt_bet$pars
    }else{
      opt_fr_coalesce <- rep(0, l_par)
    }
    
    data_output <- df_input %>%
      as.data.frame() %>%
      mutate(o_fr_B365 = opt_fr_B365,
             o_fr_P = opt_fr_P,
             o_fr_GB = opt_fr_GB,
             o_fr_avg = opt_fr_avg,
             o_fr_clsc = opt_fr_coalesce) %>%
      mutate(Div = backtesting_output$Div[i],
             season = backtesting_output$season[i],
             round = backtesting_output$round[i])
    backtesting_output$res_opt_data[[i]] <- data_output
    
  }else{
    backtesting_output$res_opt_data[[i]] <- NA
  }
  
  print(i)
  
  rm(df_input)
  rm(opt_fr_P)
  rm(opt_fr_GB)
  rm(opt_fr_B365)
  rm(opt_fr_avg)
  rm(opt_fr_coalesce)
  rm(i_prob)
  rm(i_payoff)
  rm(l_par)
}

save(backtesting_output, file = "4_backtesting/db_temp/b_lightgbm_model.RData")
