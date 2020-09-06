library(tidymodels)
library(keras)
library(tidyverse)

train_data <- other_seasons_woe %>%
  group_by(team, season, created_at, match_id) %>%
  mutate(total_goals = sum(n_goals)) %>%
  as.data.frame() %>%
  
  mutate(t_n_goals_cat = ifelse(total_goals > 2.5, "Over 2.5", "Under 2.5")) %>%
  select(-league, -team, -season, -created_at, 
         -match_id, -match_results, -n_goals_cat, -n_goals, -total_goals)

train_data_n <- recipe(t_n_goals_cat ~ ., 
                       data = train_data %>% as.data.frame()) %>%
  themis::step_upsample(t_n_goals_cat) %>%
  prep() %>%
  juice() %>%
  as.data.frame() %>%
  mutate(t_n_goals_cat = ifelse(t_n_goals_cat  == "Over 2.5", 1, 0))


label_train <- as.matrix(train_data_n %>% 
                           select(t_n_goals_cat))

val_x <- as.matrix(season_1819_woe %>% 
                     select(names(train_data_n %>% select(-t_n_goals_cat))))

val_y <- 
  as.matrix(season_1819_woe %>%
              group_by(team, season, created_at, match_id) %>%
              mutate(total_goals = sum(n_goals)) %>%
              mutate(t_n_goals_cat = ifelse(total_goals > 2.5, 1, 0)) %>%
              as.data.frame() %>%
              select(t_n_goals_cat))

# ----- Model Estimation -----
# create sequential model
model = keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(input_shape = ncol(train_data_n) - 1, 
              units = 10, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu", 
              kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.05), 
              bias_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.05)) %>%
  layer_dense(units = 50, activation = "relu",
              kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.05), 
              bias_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.05)) %>%
  layer_dense(units = 1, activation = "sigmoid")

# add a loss function and optimizer
model %>%
  compile(
    loss = "binary_crossentropy",
    optimizer = "adagrad",
    metrics = "binary_accuracy"
  )

fit = model %>%
  fit(
    x = as.matrix(train_data_n %>% select(-t_n_goals_cat)), 
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
    validation_data = list(val_x, val_y),
    epochs = 200
  )

season_1920_woe$prob <- 
  predict_proba(object = model, 
                x = as.matrix(season_1920_woe %>% 
                                select(names(train_data_glm %>% 
                                               select(-n_goals_cat)))))

season_1920_woe$class_pred <- 
  predict_classes(object = model, 
                  x = as.matrix(season_1920_woe %>% 
                                  select(names(train_data_glm %>% 
                                                 select(-n_goals_cat)))))

season_1920_woe <- season_1920_woe %>%
  select(-names(train_data_glm %>% select(-n_goals_cat)))

# - getting bookmakers odds
season_1920_woe <- 
  season_1920_woe %>%
  left_join(., t_match_stats %>% 
              select(season, league, created_at, 
                     match_id, `b365.2.5`, `b365.2.5.1`, 
                     `p.2.5`, `p.2.5.1`, `gb.2.5`, `gb.2.5.1`) %>%
              distinct()) %>%
  as.data.frame() %>%
  rowwise() %>%
  mutate(odds_25 = 
           coalesce(`b365.2.5`, `p.2.5`, `gb.2.5`),
         odds_251 = 
           coalesce(`b365.2.5.1`, `p.2.5.1`, `gb.2.5.1`)) %>%
  select(-`b365.2.5`, -`b365.2.5.1`, -`p.2.5`, 
         -`p.2.5.1`, -`gb.2.5`, -`gb.2.5.1`)
