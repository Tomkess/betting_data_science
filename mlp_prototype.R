library(tidyverse)
library(tidymodels)
library(recipes)

# ----- Upsampled Data for Modelling -----
train_data_glm <- 
  recipe(n_goals ~ ., 
         data = master_data %>%
           filter(data_type %in% "Train") %>%
           select(data_type, binned_data) %>%
           unnest(c(binned_data)) %>%
           as.data.frame() %>%
           select(-data_type, -is_home, -created_at, -match_id, - n_goals_cat) %>%
           mutate_if(is.character, as.factor)) %>%
  # themis::step_upsample(n_goals_cat) %>%
  prep() %>%
  juice() %>%
  as.data.frame()

# ----- Model Structure -----
ml_model_str <-
  
  mlp(hidden_units = tune(), dropout = tune(), 
      activation = "relu", epochs = 5) %>%
  
  set_engine("keras") %>% 
  set_mode("regression") %>%
  translate()

# ----- Stratified Sampling -----
cv_splits <- vfold_cv(train_data_glm)

# ----- Create Workflow -----
ml_workflow <- 
  workflow() %>%
  add_model(ml_model_str) %>%
  add_formula(n_goals ~ .)

# ----- Set Parameters -----
glmn_set <- parameters(hidden_units(),
                       dropout())

# ----- Create Grid -----
glmn_grid <- grid_max_entropy(glmn_set, size = 100)
ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

# ----- Tune Model -----
glmn_tune <- 
  tune_grid(ml_workflow,
            resamples = cv_splits,
            grid = glmn_grid,
            metrics = metric_set(rmse),
            control = ctrl)

# ----- Create Final Model -----
ml_model_fit <- 
  ml_workflow %>%
  finalize_workflow(select_best(glmn_tune, metric = "rmse")) %>%
  fit(., data = train_data_glm)

autoplot(glmn_tune, 
         metric = "rmse") +
  labs(title = "Results of Grid Search for Parameters of a Elastic Net") +
  ylab(label = "ROCAUC")
