# ----- Library Loading -----
library(data.table)
library(dplyr)
library(h2o)
h2o.init()

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Modelling and Match Data -----
load("2_ml_pipelines/db_temp/train_data.RData")
load("2_ml_pipelines/db_temp/test_data.RData")

# ----- Fit deeplearning model using h2o -----
traindata <- 
  train_woe %>%
  mutate(n_goals_dummy = ifelse(n_goals > 2.5, "Over 2.5", "Under 2.5")) %>%
  dplyr::select(-match_date, -match_result, -team, -n_goals) %>%
  mutate_if(is.character, factor) %>%
  as.data.frame()

valdata <- 
  test_woe %>%
  mutate(n_goals_dummy = ifelse(n_goals > 2.5, "Over 2.5", "Under 2.5")) %>%
  dplyr::select(-match_date, -match_result, -team, -n_goals) %>%
  mutate_if(is.character, factor) %>%
  as.data.frame()

traindata_h2o <- as.h2o(traindata)
valdata_h2o <- as.h2o(valdata)

fit_model_h2o <- h2o.deeplearning(x = 1:41, y = 42, 
                                  training_frame = traindata_h2o, 
                                  seed = 123456, 
                                  validation_frame = valdata_h2o, 
                                  keep_cross_validation_predictions = T, 
                                  nfolds = 50, balance_classes = T, 
                                  elastic_averaging = T, 
                                  stopping_metric = "logloss",
                                  #fold_assignment = "Stratified",
                                  standardize = T)

h2o.performance(fit_model_h2o, traindata_h2o)
h2o.performance(fit_model_h2o, valdata_h2o)
model_h2o <- h2o.saveModel(object = fit_model_h2o, 
                           path = "2_ml_pipelines/db_temp")
