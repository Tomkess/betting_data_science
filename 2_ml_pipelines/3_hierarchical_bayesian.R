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

# ----- Create Weighting Structure -----
modelling_data <- modelling_data %>%
  left_join(., match_data %>%
              group_by(created_at, is_home, team) %>%
              summarise(m_weights = abs(mean(r_bookmakers_fee))),
            by = c("match_date" = "created_at", 
                   "is_home" = "is_home", 
                   "team" = "team"))

# ----- Fit Specific Model -----
model_output <- list()
for(i in unique(modelling_data$league)){
  
  # i <- "E0"
  
  print(i)
  
  traindata <- 
    modelling_data %>%
    filter(match_date <= as.Date(train_date) & league %in% i) %>%
    dplyr::select(-match_date, -league) %>%
    
    # - increase the sample based on the weights
    mutate(i_row = row_number()) %>%
    group_by(i_row) %>%
    do(sample_n(., floor(m_weights), replace = TRUE)) %>%
    as.data.frame() %>%
    
    # - de - select the columns
    dplyr::select(-i_row, -m_weights) %>%
    as.data.frame()
  
  svd_temp <- 
    svd(traindata %>% 
          dplyr::select(-team, -match_result, -n_goals), LINPACK = FALSE)
  
  train_svd <- 
    as.matrix(svd_temp$u[, 1:15]) %*% 
    as.matrix(diag(svd_temp$d)[1:15, 1:15]) %*% 
    t(as.matrix(svd_temp$v[1:15, 1:15]))
  train_svd <- as.data.frame(train_svd)
  names(train_svd) <- paste("comp_", c(1:15), sep = "")
  
  train_svd <- train_svd %>%
    cbind(., traindata %>% 
            dplyr::select(n_goals))
  
  model_bayes_glm <- 
    bayesglm(n_goals ~ ., data = train_svd, family = poisson(link = "log"))
  
  model_output[[i]] <- model_bayes_glm
}

# ----- Fit General Model -----
traindata <- 
  modelling_data %>%
  filter(match_date <= as.Date(train_date)) %>%
  dplyr::select(-match_date, -league) %>%
  
  # - increase the sample based on the weights
  mutate(i_row = row_number()) %>%
  group_by(i_row) %>%
  do(sample_n(., floor(m_weights/2), replace = TRUE)) %>%
  as.data.frame() %>%
  
  # - de - select the columns
  dplyr::select(-i_row, -m_weights) %>%
  as.data.frame()

svd_temp <- 
  svd(traindata %>% dplyr::select(-team, -match_result, -n_goals), 
      LINPACK = FALSE)

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

model_general <- bayesglm(n_goals ~ ., 
                          data = train_svd,
                          family = poisson(link = "log"))

save(list = c("model_output", "model_general"), 
     file = "2_ml_pipelines/db_temp/5_hierarchical_bayes.RData")
