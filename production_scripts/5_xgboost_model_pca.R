# ----- Fitting XGBOOST Model -----
library(data.table)
library(dplyr)
library(tidyverse)
library(mlr)
library(knitr)
library(xgboost)
library(parallel)
library(parallelMap)
library(yardstick)
library(broom)
library(recipes)

rm(list = ls())
gc()

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Estimating Probability Model of Winner -----
load("data/production_data/3_modelling_data.RData")
gc()
set.seed(123)

names(prepared_winner) <- str_replace_all(string = names(prepared_winner), 
                                          pattern = " ", 
                                          replacement = "__")

names(prepared_winner) <- str_replace_all(string = names(prepared_winner), 
                                          pattern = "'", 
                                          replacement = "")

names(prepared_winner) <- str_replace_all(string = names(prepared_winner), 
                                          pattern = "__&", 
                                          replacement = "")

# ----- Preparing Principal Components -----
recipe_adjusted <- recipe(prepared_winner, formula = win_loss ~ .,)
recipe_adjusted <- recipe_adjusted %>%
  step_pca(contains("_10"), num_comp = 10, 
           role = "predictor", prefix = "vars_10_") %>%
  step_pca(contains("_15"), num_comp = 10, 
           role = "predictor", prefix = "vars_15_") %>%
  step_pca(contains("_20"), num_comp = 10, 
           role = "predictor", prefix = "vars_20_") %>%
  step_pca(contains("_25"), num_comp = 10, 
           role = "predictor", prefix = "vars_25_") %>%
  step_pca(contains("_30"), num_comp = 10, 
           role = "predictor", prefix = "vars_30_") %>%
  step_pca(contains("_35"), num_comp = 10, 
           role = "predictor", prefix = "vars_35_") %>%
  step_pca(contains("_40"), num_comp = 10, 
           role = "predictor", prefix = "vars_40_") %>%
  step_pca(contains("_45"), num_comp = 10, 
           role = "predictor", prefix = "vars_45_") %>%
  step_pca(contains("_50"), num_comp = 10, 
           role = "predictor", prefix = "vars_5_") %>%
  prep(retain = TRUE)

prepared_adjusted <- recipe_adjusted %>%
  prep(retain = TRUE) %>%
  bake(new_data = prepared_winner) %>%
  as.data.frame()

# ----- Create tasks -----
traintask <- makeClassifTask(data = prepared_adjusted[1:10000,] %>% 
                               as.data.frame(), 
                             target = "win_loss")

# ----- Create learner -----
lrn <- makeLearner("classif.xgboost", predict.type = "response")
lrn$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 250,
  early.stop.round = 20)

# ----- Set parameter space -----
params <- 
  makeParamSet(makeDiscreteParam("booster", values = c("gbtree")),
               makeIntegerParam("max_depth", lower = 3L, upper = 10L),
               makeNumericParam("min_child_weight", lower = 2L, upper = 10L),
               # makeNumericParam("subsample", lower = 0.5, upper = 1),
               # makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
               # makeIntegerParam("nrounds", lower = 30L, upper = 200L),
               makeNumericParam("eta", lower = 0.05, upper = 0.25))

# ----- Set resampling strategy -----
rdesc <- makeResampleDesc("CV", stratify = T, iters = 50L)

# ----- Search strategy -----
ctrl <- makeTuneControlRandom(maxit = 50L)

# ----- Set parallel backend
parallelStartSocket(cpus = 1)

# ----- Parameter tuning -----
mytune <- tuneParams(learner = lrn, 
                     task = traintask,
                     resampling = rdesc,
                     measures = acc,
                     par.set = params,
                     control = ctrl,
                     show.info = T)
