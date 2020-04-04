# ----- Modelling Data Preparation -----
library(data.table)
library(dplyr)
library(MASS)
library(tidyr)
library(purrr)
library(glmnet)

rm(list = ls())
gc()

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Load Modelling and Match Data -----
load("2_ml_pipelines/db_temp/modelling_data.RData")
load("1_variable_calculator/db_temp/1_variable_calculator.RData")

rm(predictors_data)

# ----- Model COnfiguration -----
test_date <- "2018-06-01"

# ----- Create Weighting Structure -----
modelling_data <- modelling_data %>%
  left_join(., match_data %>%
              group_by(created_at, is_home, team) %>%
              summarise(m_weights = abs(mean(r_bookmakers_fee))),
            by = c("match_date" = "created_at", 
                   "is_home" = "is_home", 
                   "team" = "team"))

# ----- Fit cv glmnet Model using xgboost package -----
traindata <- 
  modelling_data %>%
  filter(match_date <= as.Date(test_date)) %>%
  dplyr::select(-match_date, -league) %>%
  
  # - increase the sample based on the weights
  mutate(i_row = row_number()) %>%
  group_by(i_row) %>%
  do(sample_n(., floor(m_weights), replace = TRUE)) %>%
  as.data.frame() %>%
  
  # - de - select the columns
  dplyr::select(-i_row, -m_weights) %>%
  as.data.frame()

svd_temp <- svd(traindata %>% 
                  dplyr::select(-team, -match_result, -n_goals), 
                LINPACK = FALSE)

U20 <- as.matrix(svd_temp$u[, 1:20])
d20 <- as.matrix(diag(svd_temp$d)[1:20, 1:20])
V20 <- as.matrix(svd_temp$v[1:20, 1:20])

train_svd <- U20 %*% d20 %*% t(V20)
train_svd <- as.data.frame(train_svd)
names(train_svd) <- paste("comp_", c(1:20), sep = "")

normFunc <- function(x){ (x - mean(x, na.rm = T))/sd(x, na.rm = T)}

train_svd <- train_svd %>%
  mutate_all(normFunc) %>%
  cbind(., traindata %>% 
          dplyr::select(n_goals))

lambda <- 10^seq(3, -4, by = -.1)

cv_folds <- KFold(train_svd$n_goals, 
                  nfolds = 30,
                  stratified = FALSE, 
                  seed = 0)
names(cv_folds) <- c(1:n_folds_input)

foldid_input <- reshape2::melt(cv_folds) %>%
  rename(fold_n = L1) %>%
  mutate(fold_n = as.numeric(fold_n)) %>%
  arrange(value)

cv <- cv.glmnet(x = data.matrix(train_svd %>% 
                                  dplyr::select(-n_goals) %>% 
                                  as.data.frame()), 
                y = data.matrix(train_svd$n_goals), family = "poisson", 
                type.measure = "mse", foldid = foldid_input$fold_n, 
                lambda = lambda)

model_output <- cv
save(list = c("model_output"), 
     file = "2_ml_pipelines/db_temp/5_cv_glmnet_model.RData")
