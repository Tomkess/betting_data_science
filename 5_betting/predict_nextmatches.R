# ---- Library Load ----
library(dplyr)
library(data.table)
library(tidyr)
library(purrr)
library(lightgbm)
library(Matrix)
library(Rsolnp)

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Light GBM Model -----
model_lightgbm <- 
  readRDS.lgb.Booster("2_ml_pipelines/db_temp/5_lightgbm_model.rds")

# ----- Get Master Data -----
load("1_variable_calculator/db_temp/1_variable_calculator_B1.RData")
rm(predictors_data)

# ----- Load Nearest Matches -----
load("3_upcoming_matches/db_temp/nearest_matches.RData")

# ----- Load Match Data -----
load("0_etl/db_temp/0_results_download.RData")

# ----- Load Predictors Function -----
source("2_ml_pipelines/0_predictors_function.R")

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

gc()

# ----- Get Predictors Data Structure -----
n_predictors_data <- 
  
  # - Home Team matches
  nearest_matches %>%
  select(HomeTeam_tipsport, dateclosed, HomeTeam_League) %>%
  distinct() %>%
  rename(team = HomeTeam_tipsport,
         league = HomeTeam_League) %>%
  mutate(is_home = 1,
         match_date = as.Date(dateclosed),
         sport = "football") %>%
  select(-dateclosed) %>%
  
  # - Away Team matches
  rbind(., nearest_matches %>%
          select(AwayTeam_tipsport, dateclosed, AwayTeam_League) %>%
          distinct() %>%
          rename(team = AwayTeam_tipsport,
                 league = AwayTeam_League) %>%
          mutate(is_home = 0,
                 match_date = as.Date(dateclosed),
                 sport = "football") %>%
          select(-dateclosed)) %>%
  
  group_by(sport, league, match_date, is_home, team) %>%
  mutate(gp_suffix = 
           list(data.frame("gp_suffix" = c(1:10) * 5))) %>%
  unnest(c(gp_suffix)) %>%
  as.data.frame() %>%
  
  # - get dates of historical matches
  left_join(., match_data %>% 
              select(created_at, team) %>% 
              distinct() %>% 
              as.data.frame()) %>%
  
  group_by(sport, league, match_date, is_home, team, gp_suffix) %>%
  arrange(desc(created_at)) %>%
  mutate(last_n = row_number()) %>%
  
  # - subset only matches relevant for calculation
  as.data.frame() %>%
  
  filter(gp_suffix >= last_n) %>%
  rename(prev_match_date = created_at) %>%
  
  # - get suspicious data - wrong namings (double )
  group_by(sport, league, match_date, is_home, team) %>%
  mutate(max_date = max(prev_match_date)) %>%
  as.data.frame() %>%
  mutate(diff_t = 
           as.numeric(difftime(match_date, max_date, units = "days"))) %>%
  mutate(suspicious = ifelse(diff_t > 365, 1, 0)) %>%
  select(-max_date, -diff_t) %>%
  as.data.frame() %>%
  
  # - get predictors data structure
  group_by(sport, league, match_date, is_home, team, gp_suffix, suspicious) %>%
  nest() %>%
  rename(hist_match = data) %>%
  as.data.frame() %>%
  mutate(gp_suffix = paste("last_", gp_suffix, sep = ""))

# ----- Apply get_modellingdata function -----
n_modelling_data <- get_modellingdata(n_predictors_data)

# ----- create model data -----
d_lightgbm <- as(n_modelling_data %>%
                   dplyr::select(-match_date, -league, -team, -sport) %>% 
                   as.matrix(), 
                 Class = "sparseMatrix")

n_modelling_data$lightgbm_pred <- 
  as.numeric(model_lightgbm$predict(object = model_lightgbm, 
                                    data = d_lightgbm))

n_modelling_data <- n_modelling_data %>%
  select(league, team, match_date, lightgbm_pred)

n_nearest_matches <- nearest_matches %>%
  
  mutate(match_date = as.Date(dateclosed)) %>%
  left_join(., n_modelling_data, 
            by = c("match_date" = "match_date", 
                   "HomeTeam_tipsport" = "team",
                   "HomeTeam_League" = "league")) %>%
  rename(home_pred = lightgbm_pred) %>%
  
  left_join(., n_modelling_data, 
            by = c("match_date" = "match_date", 
                   "AwayTeam_tipsport" = "team",
                   "AwayTeam_League" = "league")) %>%
  rename(away_pred = lightgbm_pred) %>%
  
  left_join(., n_predictors_data %>%
              select(team, league, match_date, suspicious) %>%
              distinct(),
            by = c("match_date" = "match_date", 
                   "AwayTeam_tipsport" = "team",
                   "AwayTeam_League" = "league")) %>%
  rename(away_suspicious = suspicious) %>%
  left_join(., n_predictors_data %>%
              select(team, league, match_date, suspicious) %>%
              distinct(),
            by = c("match_date" = "match_date", 
                   "HomeTeam_tipsport" = "team",
                   "HomeTeam_League" = "league")) %>%
  rename(home_suspicious = suspicious) %>%
  rowwise() %>%
  mutate(suspicious = home_suspicious + away_suspicious) %>%
  select(-home_suspicious, -away_suspicious)

f_nearest_matches <- n_nearest_matches %>%
  select(dateclosed, namefull, sport_league, created_at, 
         tipsportA, tipsportD, tipsportH,
         `Více než 2.5`, `Méně než 2.5`, away_pred, home_pred, 
         suspicious) %>%
  filter(suspicious == 0) %>%
  as.data.frame() %>%
  
  group_by(dateclosed, namefull, sport_league) %>%
  mutate(max_createdat = max(created_at)) %>%
  as.data.frame() %>%
  filter(created_at == max_createdat) %>%
  select(-suspicious, -max_createdat) %>%
  rename(pred_A = away_pred,
         pred_H = home_pred) %>%
  
  # - create poisson distribution
  group_by(dateclosed, namefull, 
           sport_league, created_at) %>%
  nest() %>%
  rename(match_data = data) %>%
  
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
  
  group_by(sport_league) %>%
  nest() %>%
  
  group_by(sport_league) %>%
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
               mutate(payoff_Tipsport = 
                        ifelse(prob_25 > prob_251, 
                               `Méně než 2.5`, `Více než 2.5`))
             
             return(data_temp)
           }))

f_nearest_matches <- 
  f_nearest_matches %>% 
  mutate(res_opt_data = list(NA))

for(i in 1:nrow(f_nearest_matches)){
  df_input <- f_nearest_matches$opt_data_g[[i]]
  
  if(nrow(df_input) > 2){
    # - my prediction
    i_prob <- as.numeric(df_input %>% pull(my_pred))
    l_par <- length(i_prob)
    
    # - set up payoff 1
    i_payoff <- df_input %>% pull(payoff_Tipsport)
    if(sum(is.na(i_payoff)) < 1){
      opt_bet <- 
        solnp(pars = c(rep(0.01, l_par)),
              fun = sharpe_fct, 
              eqfun = allocation_budget, 
              eqB = 0.2,
              LB = c(rep(0.0, l_par)),
              UB = c(rep(1, l_par)), control = list(trace = 0))
      opt_fr_tipsport <- opt_bet$pars
    }else{
      opt_fr_tipsport <- rep(0, l_par)
    }
    
    data_output <- df_input %>%
      as.data.frame() %>%
      mutate(o_fr_tipsport = opt_fr_tipsport,
             bet_send = floor(opt_fr_tipsport * 15000))
    f_nearest_matches$res_opt_data[[i]] <- data_output
    
  }else{
    f_nearest_matches$res_opt_data[[i]] <- NA
  }
  
  print(i)
  
  rm(df_input)
  rm(opt_fr_tipsport)
  rm(i_prob)
  rm(i_payoff)
  rm(l_par)
}

# ----- Save the Betting strategy -----
save(list = c("f_nearest_matches"), 
     file = paste("5_betting/db_temp/", Sys.Date(), "_bet.RData", sep = ""))
