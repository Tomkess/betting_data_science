# ----- Modelling Data Preparation -----
library(data.table)
library(dplyr)
library(MASS)
library(arm)
library(tidyr)
library(purrr)

rm(list = ls())
gc()

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Load Modelling and Match Data -----
load("2_ml_pipelines/db_temp/modelling_data.RData")
load("1_variable_calculator/db_temp/1_variable_calculator.RData")

rm(predictors_data)

# ----- Define Neccessary Function -----
normFunc <- function(x){ (x - mean(x, na.rm = T))/sd(x, na.rm = T)}

# ----- Model COnfiguration -----
test_date <- "2018-06-01"
train_date <- "2018-01-01"

traindata <- 
  modelling_data %>%
  filter(match_date > as.Date(test_date)) %>%
  dplyr::select(-match_date, -league) %>%
  as.data.frame()

testdata <- 
  modelling_data %>%
  filter(match_date < as.Date(test_date)) %>%
  dplyr::select(-match_date, -league, -team, -match_result, -n_goals) %>%
  as.data.frame()

svd_temp <- 
  svd(traindata[1:1000,] %>% 
        dplyr::select(-team, -match_result, -n_goals), LINPACK = FALSE)

train_svd <- 
  as.matrix(svd_temp$u[, 1:15]) %*% 
  as.matrix(diag(svd_temp$d)[1:15, 1:15]) %*% 
  t(as.matrix(svd_temp$v[1:15, 1:15]))
train_svd <- as.data.frame(train_svd)
names(train_svd) <- paste("comp_", c(1:15), sep = "")

train_svd <- train_svd %>%
  mutate_all(normFunc) %>%
  cbind(., traindata %>% 
          dplyr::select(n_goals))
