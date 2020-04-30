# ----- Library Loading -----
library(data.table)
library(dplyr)
library(h2o)
h2o.init()

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

# ----- Model COnfiguration -----
test_date <- "2018-06-01"
train_date <- "2018-01-01"

# ----- Fit deeplearning model using h2o -----
traindata <- 
  modelling_data %>%
  filter(match_date <= as.Date(train_date)) %>%
  dplyr::select(-match_date, -match_result, -team, -league) %>%
  as.data.frame()

valdata <- 
  modelling_data %>%
  filter(match_date > as.Date(train_date) & 
           match_date < as.Date(test_date)) %>%
  dplyr::select(-match_date, -match_result, -team, -league) %>%
  as.data.frame()

traindata_h2o <- as.h2o(traindata)
valdata_h2o <- as.h2o(valdata)

model_h2o <- h2o.deeplearning(x = 1:261, y = 262, 
                              training_frame = traindata_h2o, 
                              seed = 123456, weights_column = "m_weights", 
                              validation_frame = valdata_h2o, 
                              keep_cross_validation_predictions = T, 
                              nfolds = 35)

# h2o.mae(object = model_h2o, valid = T, train = T)
# h2o.mse(object = model_h2o, valid = T, train = T)

# prediction_data <- as.numeric(as.data.frame(h2o.predict(object = model_h2o, 
#                                              newdata = testdata_h2o))$predict)

model_h2o <- h2o.saveModel(object = model_h2o, path = "2_ml_pipelines/db_temp")
