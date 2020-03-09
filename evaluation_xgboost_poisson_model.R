# ----- Fitting XGBOOST Model -----
library(data.table)
library(dplyr)
library(tidyverse)
library(xgboost)
library(parallel)
library(parallelMap)
library(fastDummies)
library(Rsolnp)

rm(list = ls())
gc()

which_sharpe <- "matches_sample"

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
source("functions.R")

# ----- Load The Estimated Models -----
load("model output/5_xgboost_countmodel.RData")

# ----- Get the Predicted Variable -----
load(file = "data/production_data/0_data_download.RData")
winloss_data <- master_data

# ----- Read Variables -----
load(file = "data/production_data/2_variable_calculation.RData")
master_data <- modelling_data
rm(modelling_data)

# ----- Specify Function to be optimized -----
sharpe_winner <- function(fr){
  return((-1)*as.numeric((matrix((matches_sample$prob) * 
                                   matches_sample$adjusted_odds - 
                                   (1 - matches_sample$prob), nrow = 1) %*% 
                            t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*% 
                                                           diag((matches_sample$prob) * 
                                                                  (1-matches_sample$prob) * 
                                                                  (matches_sample$adjusted_odds^2)) %*% 
                                                           t(t(matrix(fr, ncol = 1)))))))
}

sharpe_goals <- function(fr){
  return((-1)*as.numeric((matrix((matches_sample$prob_goals) * 
                                   matches_sample$odds_25 - 
                                   (1 - matches_sample$prob_goals), 
                                 nrow = 1) %*% 
                            t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*% 
                                                           diag((matches_sample$prob_goals) * 
                                                                  (1-matches_sample$prob_goals) * 
                                                                  (matches_sample$odds_25^2)) %*% 
                                                           t(t(matrix(fr, ncol = 1)))))))
}

# ----- Get Goals Data -----
goals_data <- 
  
  # - Home Team Results
  winloss_data %>%
  rowwise() %>%
  mutate(home = 1,
         weights = get_weights(B365A, B365D, B365H, 
                               BWA, BWH, BWD,
                               IWA, IWH, IWD,
                               PSA, PSH, PSD,
                               WHA, WHH, WHD,
                               VCA, VCH, VCD)) %>%
  select(created_at, Div, HomeTeam, AwayTeam, FTHG, weights, home) %>%
  rename(team = HomeTeam,
         opponent = AwayTeam,
         goals = FTHG) %>%
  as.data.frame() %>%
  
  # - Away Team Results
  rbind(., 
        winloss_data %>%
          rowwise() %>%
          mutate(home = 0,
                 weights = get_weights(B365A, B365D, B365H, 
                                       BWA, BWH, BWD,
                                       IWA, IWH, IWD,
                                       PSA, PSH, PSD,
                                       WHA, WHH, WHD,
                                       VCA, VCH, VCD)) %>%
          select(created_at, Div, HomeTeam, AwayTeam, FTAG, weights, home) %>%
          rename(team = AwayTeam,
                 opponent = HomeTeam,
                 goals = FTAG) %>%
          as.data.frame()) %>%
  group_by(.) %>%
  distinct() %>%
  as.data.frame()

master_data <- master_data %>%
  left_join(., goals_data, 
            by = c("team_input" = "team", 
                   "created_at" = "created_at")) %>%
  # - Dummy Teams
  as.data.frame() %>%
  dummy_cols(., select_columns = "team_input") %>%
  dummy_cols(., select_columns = "opponent") %>%
  mutate(year = year(created_at),
         month = month(created_at),
         quarter = quarter(created_at),
         week = week(created_at)) %>%
  dummy_cols(., select_columns = "Div") %>%
  as.data.frame()

# ----- Creating Adjusted Column Names -----
names(master_data) <- str_replace_all(string = names(master_data), 
                                      pattern = " ", 
                                      replacement = "__")

names(master_data) <- str_replace_all(string = names(master_data), 
                                      pattern = "'", 
                                      replacement = "")

names(master_data) <- str_replace_all(string = names(master_data), 
                                      pattern = "__&", 
                                      replacement = "")

names(master_data) <- str_replace_all(string = names(master_data), 
                                      pattern = "-", 
                                      replacement = "_")

output_evaluation <- list()
count <- 1
for(j in names(xgboost_model_output)){
  
  # j <- "E0"
  
  model_object <- xgboost_model_output[[j]]
  
  # ------ Get Match Program -----
  match_program <- winloss_data %>% 
    filter(Div %in% j) %>%
    mutate(adjusted_25 = coalesce(B365.2.5, P.2.5, Max.2.5, Avg.2.5),
           adjusted_251 = coalesce(B365.2.5.1, P.2.5.1, Max.2.5.1, Avg.2.5.1))
  
  # ----- Get Team Prediction -----
  match_prediction <- 
    master_data %>%
    filter(Div %in% j) %>%
    mutate(predicted_goals = get_predictions(data = ., 
                                             model_list = xgboost_model_output)) %>%
    group_by(.) %>%
    mutate(HomeTeam = ifelse(home == 1, team_input, opponent),
           AwayTeam = ifelse(home == 0, team_input, opponent),
           HomeGoals = ifelse(home == 1, predicted_goals, NA),
           AwayGoals = ifelse(home == 0, predicted_goals, NA)) %>%
    group_by(created_at, Div, HomeTeam, AwayTeam) %>%
    summarise(AwayGoals = sum(AwayGoals, na.rm = T),
              HomeGoals = sum(HomeGoals, na.rm = T)) %>%
    group_by(.) %>%
    left_join(., match_program %>% 
                select(created_at, Div, HomeTeam, AwayTeam, 
                       FTR, FTHG, FTAG, B365H, B365D, B365A, 
                       adjusted_25, adjusted_251)) %>%
    na.omit() %>%
    as.data.frame()
  
  # - Get Probabilities
  match_prediction <- match_prediction %>%
    cbind(., t(sapply(c(1:nrow(match_prediction)), 
                      simplify = T,
                      function(X) 
                        get_probabilities(.[X,]))) %>% 
            as.data.frame()) %>%
    
    as.data.frame() %>%
    rowwise() %>%
    mutate(FTR_pred = ifelse(AwayProb > HomeProb, "A", "H")) %>%
    as.data.frame() %>%
    rowwise() %>%
    mutate(prob = max(HomeProb, AwayProb, na.rm = T),
           adjusted_odds = ifelse(FTR_pred %in% "A", B365A, B365H),
           
           prob_goals = max(goals_25, goals_251),
           odds_25 = ifelse(goals_25 > goals_251, adjusted_25, adjusted_251)) %>%
    as.data.frame() %>%
    rowwise() %>%
    mutate(prob = max(HomeProb, AwayProb, na.rm = T),
           adjusted_odds = ifelse(FTR_pred %in% "A", B365A, B365H),
           
           prob_goals = max(goals_25, goals_251),
           odds_25 = ifelse(goals_25 > goals_251, adjusted_25, adjusted_251),
           ftr_25 = ifelse(FTAG + FTHG > 2.5, "over 2.5", "under 2.5"),
           ftr_25_pred = ifelse(goals_25 > goals_251, "under 2.5", "over 2.5")) %>%
    as.data.frame()
  
  # ----- Replicate Money Management System -----
  dates_match <- match_prediction %>%
    as.data.frame() %>%
    filter(created_at > as.Date("2018-06-01") & 
             created_at <= as.Date("2020-06-30")) %>%
    group_by(created_at) %>%
    summarise(total_matches = n()) %>%
    arrange(created_at) %>%
    as.data.frame() %>%
    rowwise() %>%
    mutate(result = NA,
           result_25 = NA)
  
  for(i in 1:nrow(dates_match)){
    
    # i <- 2
    matches_sample <- 
      match_prediction %>%
      filter(created_at %in% dates_match$created_at[i])
    
    if(nrow(matches_sample) >= 2){
      
      # - Get Optimized Budget
      opt_winner <- solnp(pars = c(rep(0.01, nrow(matches_sample))), 
                          fun = sharpe_winner, 
                          eqfun = allocation_budget, 
                          eqB = 0.1,
                          LB = c(rep(0.0, nrow(matches_sample))),
                          UB = c(rep(1, nrow(matches_sample))))
      
      opt_goals <- solnp(pars = c(rep(0.01, nrow(matches_sample))), 
                         fun = sharpe_goals, 
                         eqfun = allocation_budget, 
                         eqB = 0.1,
                         LB = c(rep(0.0, nrow(matches_sample))),
                         UB = c(rep(1, nrow(matches_sample))))
      
      matches_sample$fr_winner <- opt_winner$pars
      matches_sample$fr_goals <- opt_goals$pars
    }else{
      matches_sample$fr_winner <- 0.01
      matches_sample$fr_goals <- 0.01
    }
    
    output_evaluation[[count]] <- matches_sample
    
    count <- count + 1
    print(i)
  }
}

evaluation_masterdata <- bind_rows(output_evaluation)

library(ggplot2)
ggplot(data = evaluation_masterdata %>%
         filter(Div %in% "E1") %>%
         rowwise() %>%
         mutate(win_match = ifelse(FTR_pred == FTR, 
                                   fr_winner * (adjusted_odds - 1), 
                                   fr_winner * (-1)),
                win_goals = ifelse(ftr_25 == ftr_25_pred, 
                                   fr_goals * (odds_25 -1),
                                   fr_goals * (-1))) %>%
         group_by(Div, created_at) %>%
         summarise(wealth_goals_new = sum(win_goals),
                   wealth_winner_new = sum(win_match)) %>%
         as.data.frame() %>%
         group_by(Div) %>%
         arrange(created_at) %>%
         mutate(wealth_goals = cumsum(wealth_goals_new),
                wealth_winner = cumsum(wealth_winner_new)) %>%
         group_by(created_at)) +
  geom_line(aes(x = created_at, y = wealth_goals, col = "# Goals Bet")) + 
  geom_line(aes(x = created_at, y = wealth_winner, col = "Winner Bet"))
