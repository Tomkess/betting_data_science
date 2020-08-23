# ----- Loading the Library -----
library(data.table)
library(dplyr)
library(tidymodels)
library(tidyr)
library(vip)

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Modeling and Match Data -----
load("2_ml_pipelines/db_temp/train_data.RData")
load("2_ml_pipelines/db_temp/test_data.RData")

match_train <- train_woe %>%
  mutate(n_goals_dummy = ifelse(n_goals > 2.5, "over 2.5", "under 2.5")) %>%
  dplyr::select(-match_date, -n_goals, -match_result, -team) %>%
  mutate_if(is.character, factor)

match_test <- test_woe %>%
  mutate(n_goals_dummy = ifelse(n_goals > 2.5, "over 2.5", "under 2.5")) %>%
  dplyr::select(-match_date, -n_goals, -match_result, -team) %>%
  mutate_if(is.character, factor)

xgb_spec <- 
  boost_tree(
  trees = tune(),
  stop_iter = 5, 
  tree_depth = tune(), 
  loss_reduction = tune(),
  mtry = tune(),
  learn_rate = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_grid <- grid_latin_hypercube(
  trees(range = c(20, 40)),
  tree_depth(range = c(1, 5)),
  loss_reduction(),
  finalize(mtry(), match_train),
  learn_rate(),
  size = 200)

xgb_wf <- workflow() %>%
  add_formula(n_goals_dummy ~ .) %>%
  add_model(xgb_spec)

set.seed(123)
match_folds <- vfold_cv(match_train, strata = "n_goals_dummy", v = 25)

xgb_res <- tune_grid(
  xgb_wf,
  resamples = match_folds,
  grid = xgb_grid,
  metrics = yardstick::metric_set(pr_auc, roc_auc, recall, mn_log_loss),
  control = control_grid(save_pred = TRUE))

# ----- Performance Visualization -----

# pr_auc
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "pr_auc") %>%
  select(mean, mtry:loss_reduction) %>%
  pivot_longer(mtry:loss_reduction,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Area under Precision and Recall Curve")

# roc_auc
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry:loss_reduction) %>%
  pivot_longer(mtry:loss_reduction,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Area under Curve (ROC)")

# recall
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "recall") %>%
  select(mean, mtry:loss_reduction) %>%
  pivot_longer(mtry:loss_reduction,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Recall")

# mn_log_loss
xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "mn_log_loss") %>%
  select(mean, mtry:loss_reduction) %>%
  pivot_longer(mtry:loss_reduction,
               values_to = "value",
               names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "Mean LogLoss")

# ----- Select Best Configuration -----
best_acc <- select_best(xgb_res, "roc_auc")
final_xgb <- finalize_workflow(
  xgb_wf,
  best_acc)

final_xgb %>%
  fit(data = match_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point", num_features = 10)

master_data <- rbind(match_train, match_test)
data_split <- initial_split(master_data, prop = 3/4, strata = "n_goals_dummy")

final_res <- last_fit(final_xgb, split = data_split)
collect_metrics(final_res)

final_res %>%
  collect_predictions() %>%
  roc_curve(n_goals_dummy, `.pred_over 2.5`) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "midnightblue") +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2)

save.image("2_ml_pipelines/db_temp/5_xgboost_countmodel.RData")
