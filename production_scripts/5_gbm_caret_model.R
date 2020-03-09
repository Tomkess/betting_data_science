# ----- Library Loading -----
library(data.table)
library(dplyr)
library(rsample)
library(caret)
library(xgboost)
library(recipes)
library(tidymodels)
library(furrr)
library(doParallel)
library(foreach)
library(gbm)

rm(list = ls())
gc()

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Estimating Probability Model of Winner -----
load("data/production_data/3_modelling_data.RData")
gc()
set.seed(123)

# ----- Train GBM model -----
gbm_fit <- 
  gbm(formula = win_loss ~ .,
      distribution = "gaussian",
      
      # - Testing the subset of a data
      # data = prepared_winner[sample(x = 1:nrow(prepared_winner), 
      #                               size = 25000, 
      #                               replace = F),],
      
      # - running the script on whole data sample
      data = prepared_winner,
      n.trees = 1000,
      interaction.depth = 1,
      shrinkage = 0.001,
      cv.folds = 20,
      n.cores = NULL,
      verbose = FALSE)

# ----- saving the Models Output -----
save.image("data/production_data/5_model_estimation.RData")
