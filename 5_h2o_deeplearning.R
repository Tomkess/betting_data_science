library(h2o)
h2o.init()

# ----- Fit bayesian glm model using the package arm -----
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

save(list = c("model_h2o"), 
     file = "2_ml_pipelines/db_temp/5_h2o_deeplearning.RData")
