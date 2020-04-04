# ----- Modelling Data Preparation -----
library(data.table)
library(dplyr)
library(lightgbm)
library(Matrix)

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Load Modelling and Match Data -----
load("2_ml_pipelines/db_temp/modelling_data.RData")
load("1_variable_calculator/db_temp/1_variable_calculator.RData")

rm(predictors_data)

# ----- Model Configuration -----
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

# ----- Fit ligth gbm model from package lightgbm -----
traindata <- 
  modelling_data %>%
  filter(match_date <= as.Date(train_date)) %>%
  
  dplyr::select(-match_date) %>%
  
  # - increase the sample based on the weights
  mutate(i_row = row_number()) %>%
  group_by(i_row) %>%
  do(sample_n(., floor(m_weights), replace = TRUE)) %>%
  as.data.frame() %>%
  
  # - de - select the columns
  dplyr::select(-i_row, -m_weights) %>%
  as.data.frame()

testdata <- 
  modelling_data %>%
  filter(match_date > as.Date(train_date) & 
           match_date <= as.Date(test_date)) %>%
  dplyr::select(-match_date) %>%
  
  # - de - select the columns
  dplyr::select(-m_weights) %>%
  as.data.frame()

rm(modelling_data)
rm(match_data)
gc()

# ----- create model data -----
dtrain <- 
  lgb.Dataset(data = as(traindata %>% 
                          select(-match_result, -n_goals) %>% 
                          as.matrix(), 
                        Class = "sparseMatrix"), 
              label = traindata$n_goals, 
              categorical_feature = c("team", "league"))

dtest <- 
  lgb.Dataset.create.valid(dtrain, 
                           data = as(testdata %>% 
                                       select(-match_result, -team, 
                                              -n_goals) %>% 
                                       as.matrix(), 
                                     Class = "sparseMatrix"), 
                           label = testdata$n_goals, 
                           categorical_feature = c("team", "league"))

# ----- Set Parameters -----
valids <- list(test = dtest)
params <- list(objective = "poisson", 
               metric = "poisson", 
               boosting = "dart",
               max_depth = 4,
               num_leaves = 15)

# ----- Train Model -----
model <- 
  lgb.train(params = params
            , data = dtrain
            , nrounds = 100L
            , valids = valids
            , min_data = 1L
            , learning_rate = 0.25
            , early_stopping_rounds = 5L)

# - Get the interpretation of the model for each row

# interpretation <-
#   lgb.interprete(model = model,
#                  data = as(testdata %>%
#                              select(-match_result, -team, -n_goals) %>%
#                              as.matrix(),
#                            Class = "sparseMatrix"), idxset = 1:20)

# lgb.plot.interpretation(interpretation[[2]], top_n = 10)

# json_model <- lgb.dump(model)

saveRDS.lgb.Booster(model, "2_ml_pipelines/db_temp/5_lightgbm_model.rds")
