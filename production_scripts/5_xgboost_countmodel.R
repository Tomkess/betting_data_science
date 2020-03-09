# ----- Fitting XGBOOST Model -----
library(data.table)
library(dplyr)
library(tidyverse)
library(xgboost)
library(parallel)
library(parallelMap)
library(yardstick)
library(broom)
library(recipes)
library(fastDummies)
library(rBayesianOptimization)

rm(list = ls())
gc()
which_sharpe <- "xml_data"

# ----- Model COnfiguration -----
test_date <- "2018-06-01"
train_date <- "2018-01-01"

# ----- Functions -----
xgb_cv_bayes <- function(eta, max_depth, min_child_weight, 
                         n_folds_input = 30, gamma, alpha, 
                         lambda) {
  
  cv_folds <- KFold(xgboost_traindata$goals, 
                    nfolds = n_folds_input,
                    stratified = FALSE, 
                    seed = 0)
  
  # eta = 0.15
  # max_depth = 2
  # min_child_weight = 3
  # n_folds_input = 30
  # lambda = 1
  # gamma = 0
  # alpha = 1
  
  cv <- xgb.cv(params = list(booster = "gbtree", 
                             eta = eta,
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = 1, 
                             colsample_bytree = 1,
                             lambda = lambda,
                             alpha = alpha,
                             gamma = gamma,
                             objective = "count:poisson",
                             eval_metric = "poisson-nloglik"),
               
               data = dtrain,
               watchlist = list(test = dtest),
               nrounds = 25,
               print_every_n = 10,
               folds = cv_folds, 
               prediction = F, 
               showsd = F,
               early_stopping_rounds = 5,
               verbose = T)
  
  max_nrounds <- length(cv$evaluation_log$test_poisson_nloglik_mean)
  
  list(Score = (-1) * 
         cv$evaluation_log$test_poisson_nloglik_mean[max_nrounds],
       Pred = cv$pred)
}

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
source("functions.R")

# ----- Get the Predicted Variable -----
load(file = "data/production_data/0_data_download.RData")
winloss_data <- master_data

# ----- Read Variables -----
load(file = "data/production_data/2_variable_calculation.RData")
master_data <- modelling_data
rm(modelling_data)

goals_data <- 
  
  # - Home Team Results
  winloss_data %>%
  rowwise() %>%
  mutate(adjusted_D = coalesce(B365D, BWD, IWD, PSD, WHD, VCD, LBD, SJD, GBD, 
                               BSD, SBD, SOD, SYD),
         adjusted_H = coalesce(B365H, BWH, IWH, PSH, WHH, VCH, LBH, SJH, GBH, 
                               BSH, SBH, SOH, SYH),
         adjusted_A = coalesce(B365A, BWA, IWA, PSA, WHA, VCA, LBA, SJA, GBA, 
                               BSA, SBA, SOA, SYA)) %>%
  select(created_at, Div, HomeTeam, AwayTeam, FTHG, adjusted_H, adjusted_D, 
         adjusted_A) %>%
  group_by(.) %>%
  mutate(home = 1,
         weights = (-1) *(100 - (100/adjusted_D + 100/adjusted_H + 
                                   100/adjusted_A))) %>%
  rename(team = HomeTeam,
         opponent = AwayTeam,
         goals = FTHG) %>%
  as.data.frame() %>%
  
  # - Away Team Results
  rbind(., 
        winloss_data %>%
          rowwise() %>%
          mutate(adjusted_D = coalesce(B365D, BWD, IWD, PSD, WHD, VCD, LBD, SJD, 
                                       GBD, BSD, SBD, SOD, SYD),
                 adjusted_H = coalesce(B365H, BWH, IWH, PSH, WHH, VCH, LBH, SJH, 
                                       GBH, BSH, SBH, SOH, SYH),
                 adjusted_A = coalesce(B365A, BWA, IWA, PSA, WHA, VCA, LBA, SJA, 
                                       GBA, BSA, SBA, SOA, SYA)) %>%
          select(created_at, Div, HomeTeam, AwayTeam, FTAG, adjusted_H, 
                 adjusted_D, adjusted_A) %>%
          group_by(.) %>%
          mutate(home = 0,
                 weights = (-1) *(100 - (100/adjusted_H + 100/adjusted_D + 
                                           100/adjusted_A))) %>%
          rename(team = AwayTeam,
                 opponent = HomeTeam,
                 goals = FTAG) %>%
          as.data.frame()) %>%
  group_by(.) %>%
  distinct() %>%
  select(-adjusted_H, -adjusted_D, -adjusted_A) %>%
  as.data.frame()

master_data <- 
  master_data %>%
  left_join(., goals_data, 
            by = c("team_input" = "team", "created_at" = "created_at"))

# ----- Dummy Teams -----
master_data <- master_data %>%
  as.data.frame() %>%
  dummy_cols(., select_columns = "team_input") %>%
  # dummy_cols(., select_columns = "opponent") %>%
  mutate(year = year(created_at),
         month = month(created_at),
         quarter = quarter(created_at),
         week = week(created_at)) %>%
  dummy_cols(., select_columns = "Div") %>%
  as.data.frame()

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

rm(winloss_data)
rm(goals_data)
gc()

# ----- Create tasks -----
xgboost_model_output <- list()
# removed_leagues <- c("E0", "E1", "E2", "E3", "EC", "SC0", "SC1", "SC2", 
# "SC3", "SP2", "D1",  "D2",  "I1",  "I2",  "SP1", "F1", "F2", "N1", "P1", "T1")
removed_leagues <- c("")
for(i in as.character(na.omit(unique(master_data$Div)))[!(as.character(na.omit(unique(master_data$Div))) %in% removed_leagues)]){
  
  # i <- "D1"
  
  print(i)
  print(Sys.time())
  
  # ----- Fit XGBOOST Model using xgboost package -----
  xgboost_traindata <- 
    master_data %>%
    filter(created_at <= as.Date(train_date) & 
             Div %in% i & 
             !(is.infinite(weights))) %>%
    as.data.frame() %>%
    select(-created_at, -team_input, -Div, -opponent) %>%
    na.omit() %>%
    as.data.frame() %>%
    mutate(i_row = row_number()) %>%
    group_by(i_row) %>%
    do(sample_n(., floor(weights), replace = TRUE)) %>%
    as.data.frame() %>%
    select(-i_row, -weights) %>%
    as.data.frame() %>%
    select(names(which(colSums(.) > 0))) %>%
    as.data.frame()
  
  xgboost_testdata <- 
    master_data %>%
    filter(created_at <= as.Date(test_date) &
             created_at >= as.Date(train_date) & Div %in% i) %>%
    as.data.frame() %>%
    select(-created_at, -team_input, -Div, -opponent) %>%
    na.omit() %>%
    as.data.frame() %>%
    select(names(xgboost_traindata)) %>%
    as.data.frame()
  
  dtrain <- 
    xgb.DMatrix(data.matrix(xgboost_traindata %>% 
                              select(-goals) %>% 
                              as.data.frame()), 
                label = xgboost_traindata$goals, 
                missing = NA)
  
  dtest <- 
    xgb.DMatrix(data.matrix(xgboost_testdata %>% 
                              select(-goals) %>% 
                              as.data.frame()), 
                label = xgboost_testdata$goals, 
                missing = NA)
  
  if(nrow(xgboost_testdata) > 0 & 
     nrow(xgboost_traindata) > 0 & 
     nrow(xgboost_traindata) > ncol(xgboost_traindata)){
    
    OPT_Res <- 
      BayesianOptimization(xgb_cv_bayes,
                           bounds = list(eta = c(0.01, 0.35),
                                         max_depth = c(2L, 6L),
                                         min_child_weight = c(1L, 10L),
                                         gamma = c(0.0, 5.0),
                                         alpha = c(0.0, 5.0),
                                         lambda = c(0.0, 5.0)),
                           init_grid_dt = NULL, 
                           init_points = 20, 
                           n_iter = 20,
                           acq = "ucb", 
                           kappa = 2.576, 
                           eps = 0.0,
                           verbose = TRUE)
    
    eta_opt <- OPT_Res$Best_Par[["eta"]]
    maxdepth_opt <- OPT_Res$Best_Par[["max_depth"]]
    minchildweight_opt <- OPT_Res$Best_Par[["min_child_weight"]]
    gamma_opt <- OPT_Res$Best_Par[["gamma"]]
    lambda_opt <- OPT_Res$Best_Par[["lambda"]]
    alpha_opt <- OPT_Res$Best_Par[["alpha"]]
    
    cv_opt <- xgb.train(params = list(booster = "gbtree", 
                                      eta = eta_opt,
                                      max_depth = maxdepth_opt,
                                      min_child_weight = minchildweight_opt,
                                      subsample = 1, 
                                      colsample_bytree = 1,
                                      lambda = lambda_opt, 
                                      alpha = alpha_opt,
                                      gamma = gamma_opt,
                                      objective = "count:poisson",
                                      eval_metric = "poisson-nloglik"),
                        
                        data = dtrain,
                        watchlist = list(test = dtest), 
                        nrounds = 250,
                        showsd = TRUE,
                        early_stopping_rounds = 5, 
                        maximize = F, 
                        verbose = T)
    
    xgboost_model_output[[i]] <- cv_opt
    save(list = c("xgboost_model_output"), 
         file = "model output/5_xgboost_countmodel.RData")
  }
  
  print(Sys.time())
  print(i)
}

rm(list = ls()[!(ls() %in% c("xgboost_model_output"))])
gc()

save.image("model output/5_xgboost_countmodel.RData")
