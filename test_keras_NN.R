library(tidymodels)
library(keras)
library(tidyverse)

# ----- Upsampled Data for Modelling -----
train_data_glm <- 
  recipe(n_goals ~ ., 
         data = master_data %>%
           filter(data_type %in% "Train") %>%
           select(data_type, binned_data) %>%
           unnest(c(binned_data)) %>%
           as.data.frame() %>%
           select(-data_type, -is_home, -created_at, -match_id) %>%
           mutate_if(is.character, as.factor)) %>%
  themis::step_upsample(n_goals_cat) %>%
  prep() %>%
  juice() %>%
  as.data.frame() %>%
  mutate(n_goals_cat = ifelse(n_goals_cat == "Under 2.5", 0, 1))

val_data_glm <- 
  recipe(n_goals ~ ., 
         data = master_data %>%
           filter(data_type %in% "Val") %>%
           select(data_type, binned_data) %>%
           unnest(c(binned_data)) %>%
           as.data.frame() %>%
           select(-data_type, -is_home, -created_at, -match_id) %>%
           mutate_if(is.character, as.factor)) %>%
  step_bagimpute(woe.team.binned) %>%
  prep() %>%
  juice() %>%
  as.data.frame() %>%
  mutate(n_goals_cat = ifelse(n_goals_cat == "Under 2.5", 0, 1))


label_train <- as.matrix(train_data_glm %>% select(n_goals_cat))
label_val <- as.matrix(val_data_glm %>% select(n_goals_cat))

val_data_glm <- as.matrix(val_data_glm %>% select(-n_goals_cat, -n_goals))

# ----- Model Estimation -----
model = keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(input_shape = ncol(train_data_glm) - 2, 
              units = 16, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu", 
              kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01), 
              bias_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 32, activation = "relu",
              kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01), 
              bias_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 16, activation = "relu",
              kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01), 
              bias_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
  layer_dense(units = 1, activation = "sigmoid")

# add a loss function and optimizer
model %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

fit = model %>%
  fit(
    x = as.matrix(train_data_glm %>% select(-n_goals_cat, -n_goals)), 
    y = label_train,
    shuffle = T,
    callbacks = callback_early_stopping(
      monitor = "val_loss",
      min_delta = 0.01,
      patience = 10,
      verbose = 0,
      mode = "auto",
      baseline = NULL,
      restore_best_weights = FALSE
    ),
    batch_size = 32,
    validation_data = list(val_data_glm, label_val),
    epochs = 5
  )

