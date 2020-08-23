# ----- Modelling Data Preparation -----
library(data.table)
library(dplyr)
library(lightgbm)
library(Matrix)

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Modelling and Match Data -----
load("2_ml_pipelines/db_temp/test_data.RData")
load("2_ml_pipelines/db_temp/train_data.RData")
load("1_variable_calculator/db_temp/1_variable_calculator_B1.RData")

rm(predictors_data)
rm(match_data)
gc()

# ----- create model data -----
dtrain <- 
  lgb.Dataset(data = as(train_woe %>% 
                          select(-match_result, -n_goals,
                                 -match_date, -team) %>% 
                          as.data.frame() %>%
                          as.matrix(), 
                        Class = "sparseMatrix"), 
              label = train_woe$n_goals)

dtest <- 
  lgb.Dataset.create.valid(dtrain, 
                           data = as(test_woe %>% 
                                       select(-match_result, -n_goals,
                                              -match_date, -team) %>% 
                                       as.data.frame() %>%
                                       as.matrix(), 
                                     Class = "sparseMatrix"), 
                           label = test_woe$n_goals)

# ----- Set Parameters -----
valids <- list(test = dtest)
params <- list(objective = "poisson", 
               metric = "poisson", 
               boosting = "gbdt",
               max_depth = 4,
               num_leaves = 15)

# ----- Train Model -----
model <- lgb.train(params = params
                   , data = dtrain
                   , nrounds = 100L
                   , valids = valids
                   , min_data = 1L
                   , learning_rate = 0.25
                   , early_stopping_rounds = 5L)

gc()
saveRDS.lgb.Booster(model, "2_ml_pipelines/db_temp/5_lightgbm_model.rds")
