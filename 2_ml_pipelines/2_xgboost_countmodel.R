# ----- Loading the Library -----
library(data.table)
library(dplyr)
library(tidyverse)
library(xgboost)
library(rBayesianOptimization)

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Modelling and Match Data -----
load("2_ml_pipelines/db_temp/modelling_data.RData")
load("1_variable_calculator/db_temp/1_variable_calculator_B1.RData")

rm(predictors_data)

# ----- Model Configuration -----
test_date <- "2018-06-01"
train_date <- "2018-01-01"

# ----- Functions -----
xgb_cv_bayes <- function(eta, max_depth, min_child_weight, n_folds_input = 30, 
                         gamma, alpha, lambda) {
  
  cv_folds <- KFold(xgboost_traindata$n_goals, 
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

# ----- Create Weighting Structure -----
modelling_data <- modelling_data %>%
  left_join(., match_data %>%
              group_by(created_at, is_home, team) %>%
              summarise(m_weights = abs(mean(r_bookmakers_fee))),
            by = c("match_date" = "created_at", 
                   "is_home" = "is_home", 
                   "team" = "team"))

# ----- Fit XGBOOST Model using xgboost package -----
xgboost_traindata <- 
  modelling_data %>%
  filter(match_date <= as.Date(train_date)) %>%
  select(-match_date, -league) %>%
  as.data.frame()

xgboost_testdata <- 
  modelling_data %>%
  filter(match_date <= as.Date(test_date) &
           match_date >= as.Date(train_date)) %>%
  select(-match_date, -m_weights, -league) %>%
  as.data.frame()

dtrain <- 
  xgb.DMatrix(data.matrix(xgboost_traindata %>% 
                            select(-n_goals, -match_result, -team, -m_weights) %>% 
                            as.data.frame()), 
              label = xgboost_traindata$n_goals, 
              weight = xgboost_traindata$m_weights,
              missing = NA)

dtest <- 
  xgb.DMatrix(data.matrix(xgboost_testdata %>% 
                            select(-n_goals, -match_result, -team) %>% 
                            as.data.frame()), 
              label = xgboost_testdata$n_goals, 
              missing = NA)
gc()

rm(modelling_data)
rm(match_data)
gc()

# ----- Deprecated: too much computationally intensive ----- #
# OPT_Res <- 
#   BayesianOptimization(xgb_cv_bayes,
#                        bounds = list(eta = c(0.01, 0.2),
#                                      max_depth = c(2L, 6L),
#                                      min_child_weight = c(1L, 10L),
#                                      gamma = c(0.0, 5.0),
#                                      alpha = c(0.0, 5.0),
#                                      lambda = c(0.0, 5.0)),
#                        init_grid_dt = NULL, 
#                        init_points = 20, 
#                        n_iter = 20,
#                        acq = "ucb", 
#                        kappa = 2.576, 
#                        eps = 0.0,
#                        verbose = TRUE)

# eta_opt <- OPT_Res$Best_Par[["eta"]]
# maxdepth_opt <- OPT_Res$Best_Par[["max_depth"]]
# minchildweight_opt <- OPT_Res$Best_Par[["min_child_weight"]]
# gamma_opt <- OPT_Res$Best_Par[["gamma"]]
# lambda_opt <- OPT_Res$Best_Par[["lambda"]]
# alpha_opt <- OPT_Res$Best_Par[["alpha"]]

eta_opt <- 0.2
maxdepth_opt <- 4
minchildweight_opt <- 5
gamma_opt <- 0
lambda_opt <- 0
alpha_opt <- 0

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

xgboost_model_output <- cv_opt

rm(list = ls()[!(ls() %in% c("xgboost_model_output"))])
gc()

save.image("2_ml_pipelines/db_temp/5_xgboost_countmodel.RData")
