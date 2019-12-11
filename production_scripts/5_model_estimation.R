# ----- Library Loading -----
library(data.table)
library(dplyr)
library(Rsolnp)
library(ggplot2)
library(rsample)
library(parsnip)
library(recipes)
library(dials)
library(tidymodels)
library(furrr)
library(doParallel)
library(foreach)

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Estimating Probability Model of Winner -----
load("data/production_data/3_modelling_data.RData")

gc()

fit_on_fold <- function(spec, prepped) {
  
  x <- juice(prepped, all_predictors())
  y <- juice(prepped, all_outcomes())
  
  fit_xy(spec, x, y)
}

predict_helper <- function(fit_input, new_data, recipe_input){
  
  if (inherits(new_data, "rsplit")) {
    obs <- as.integer(new_data, data = "assessment")
    
    new_data <- bake(recipe_input, assessment(new_data))
  } else {
    obs <- 1:nrow(new_data)
    new_data <- bake(recipe_input, new_data)
  }
  hold_on <- new_data$win_loss
  new_data$win_loss <- NULL
  
  pred_model <- predict(fit_input, new_data, type = "prob") %>%
    mutate(obs = obs)
  return(pred_model)
}

# ----- Hyperparameter Tuning -----
bst_grid <- grid_random(
  trees %>%       range_set(c(1,  15)),
  min_n %>%       range_set(c(2,  30)),
  tree_depth %>% range_set(c(1, 10)),
  learn_rate %>% range_set(c(0.01, 0.25)),
  size = 100)

xgboost_model <- boost_tree(mode = "classification",
                            mtry = 20, 
                            sample_size = 0.7, 
                            trees = varying(),
                            min_n = varying(),
                            tree_depth = varying(),
                            learn_rate = varying(),
                            loss_reduction = 0.01) %>%
  set_engine(engine = "xgboost")
folds <- vfold_cv(prepared_winner, v = 30, strata = "win_loss")

# ----- Model Estimation -----
# cl <- makeCluster(2)
# registerDoParallel(cl)

performance_list <- list()
for(i in 1:nrow(bst_grid)){
# performance_comparison <-
#   foreach(i = 1:nrow(bst_grid),
#           .combine = c,
#           .packages = c("data.table", "dplyr",
#                         "tidymodels", "furrr", "purrr", "tidyr")) %dopar% {
            
            # i <- 1
            spec_df <- tibble(spec = merge(xgboost_model, bst_grid[i,])) %>% 
              mutate(model_id = "XGBoost")
            
            temp_output <- list()
            for(j in 1:nrow(folds)){
              
              # j <- 1
              crossed <- crossing(folds[j,], spec_df)
              crossed$grid_id <- i
              crossed$fold_id <- j
              
              crossed_temp <- crossed
              cv_fits_temp <- crossed_temp %>%
                mutate(
                  prepped = future_map(splits, prepper, model_recipe_winner),
                  fit = future_map2(spec, prepped, fit_on_fold))
              
              holdout_preds_temp <- cv_fits_temp %>% 
                mutate(preds = future_pmap(list(fit, splits, prepped), predict_helper)) %>%
                unnest(preds) %>%
                left_join(., prepared_winner %>% select(win_loss) %>% mutate(obs = 1:n())) %>%
                mutate(predicted = as.factor(ifelse(.pred_W > .pred_L, "W", "L")))
              
              temp_output[[j]] <- holdout_preds_temp %>%
                group_by(id, model_id, grid_id, fold_id) %>%
                roc_auc(win_loss, .pred_W) %>%
                as.data.frame()
            }
            
            # - get predictions on test data
            fit_training <- fit_on_fold(spec = spec_df$spec[[1]], 
                                        prepped = model_recipe_winner %>% 
                                          prep(retain = TRUE))
            
            # predict_test <- predict_helper(fit_input = fit_training, 
            #                                new_data = prepared_winner, 
            #                                recipe_input = model_recipe_winner)
            # predict_test$grid_id <- i
            # predict_test <- as.data.frame(predict_test)
            
            predict_train <- predict_helper(fit_input = fit_training, 
                                            new_data = prepared_winner, 
                                            recipe_input = model_recipe_winner)
            predict_train$grid_id <- i
            predict_train <- as.data.frame(predict_train)
            
            training_performance <- as.data.frame(bind_rows(temp_output))
            
            output_list <- list()
            output_list[[1]] <- list()
            output_list[[1]][["training_performance"]] <- training_performance
            # output_list[[1]][["test_prediction"]] <- predict_test
            output_list[[1]][["train_prediction"]] <- predict_train
            output_list[[1]][["model_fit_object"]] <- fit_training
            
            performance_list[[i]] <- output_list
            print(i)
            print(mean(training_performance$.estimate))
            # return(output_list)
          }
# stopCluster(cl)

rm(folds)

# ----- saving the Models Output -----
save.image("data/production_data/5_model_estimation.RData")
