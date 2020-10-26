xgboost_model <- 
  
  function(traind_df, n_folds, final_metric, set_obj,
           xgboost_mode, strata_var, target_var,
           output_objects = c("parameters", "final_model", "glmn_tune")){
    
    # ----- Create Formula -----
    get_formula <- as.formula(paste(target_var, " ~ .", sep = ""))
    
    # ----- Model Structure -----
    ml_model_str <-
      
      boost_tree(mtry = tune(), trees = tune(), min_n = tune(), 
                 tree_depth = tune(), learn_rate = tune(), 
                 loss_reduction = tune(), stop_iter = 5) %>%
      
      set_engine("xgboost", objective = set_obj) %>% 
      set_mode(xgboost_mode)
    
    # ----- Stratified Sampling -----
    cv_splits <- vfold_cv(train_df, v = n_folds, strata = strata_var)
    
    # ----- Create Work flow -----
    ml_workflow <- 
      workflow() %>%
      add_model(ml_model_str) %>%
      add_formula(get_formula)
    
    # ----- Set Parameters -----
    xgboostParams <- parameters(
      min_n(),
      tree_depth(range = c(2, 4)),
      trees(range = c(10, 40)),
      loss_reduction(),
      learn_rate(),
      finalize(mtry(), train_df))
    
    # ----- Create Grid -----
    glmn_grid <- grid_max_entropy(xgboostParams, size = 50)
    ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)
    
    # ----- Tune Model -----
    if(elnet_mode == "classification"){
      
      glmn_tune <- 
        tune_grid(ml_workflow,
                  resamples = cv_splits,
                  grid = glmn_grid,
                  metrics = metric_set(roc_auc, pr_auc, accuracy, 
                                       precision, recall),
                  control = ctrl)
      
    }else{
      
      glmn_tune <- 
        tune_grid(ml_workflow,
                  resamples = cv_splits,
                  grid = glmn_grid,
                  metrics = metric_set(rmse, mape, smape, rsq_trad),
                  control = ctrl)
      
    }
    
    # ----- Create Final Model -----
    ml_model_fit <- 
      ml_workflow %>%
      finalize_workflow(select_best(glmn_tune, metric = final_metric)) %>%
      fit(., data = train_df)
    
    # ----- Get Output -----
    output <- list()
    if ("parameters" %in% output_objects) {
      output[["parameters"]] <-
        ml_model_fit %>%
        pull_workflow_fit() %>%
        tidy() %>%
        as.data.frame()
    }
    
    if ("final_model" %in% output_objects) {
      output[["final_model"]] <- ml_model_fit
    }
    
    if ("glmn_tune") {
      output[["glmn_tune"]] <- glmn_tune
    }
    
    return(output)
    
  }
