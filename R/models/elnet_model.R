elnet_model <- 
  function(train_df, n_folds, final_metric, 
           elnet_mode, strata_var, target_var,
           output_objects = c("parameters", "final_model", "glmn_tune")){
    
    # ----- Create Formula -----
    get_formula <- as.formula(paste(target_var, " ~ .", sep = ""))
    
    # ----- Model Structure -----
    ml_model_str <-
      
      logistic_reg(penalty = tune(),
                   mixture = tune()) %>% 
      
      set_engine("glmnet") %>% 
      set_mode(elnet_mode)
    
    # ----- Stratified Sampling -----
    if(elnet_mode %in% "regression"){
      cv_splits <- vfold_cv(train_df, v = n_folds)
    }
    
    if(elnet_mode %in% "classification"){
      cv_splits <- 
        vfold_cv(train_df, strata = all_of(strata_var), v = n_folds)
    }
    
    # ----- Create Workflow -----
    ml_workflow <- 
      workflow() %>%
      add_model(ml_model_str) %>%
      add_formula(get_formula)
    
    # ----- Set Parameters -----
    glmn_set <- parameters(penalty(range = c(-5, 1), trans = log10_trans()),
                           mixture(range = c(0, 1)))
    
    # ----- Create Grid -----
    glmn_grid <- grid_regular(glmn_set, levels = c(7, 5))
    ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)
    
    # ----- Tune Model -----
    if(elnet_mode %in% "classification"){
      
      glmn_tune <- 
        tune_grid(ml_workflow,
                  resamples = cv_splits,
                  grid = glmn_grid,
                  metrics = metric_set(roc_auc, pr_auc, accuracy, 
                                       precision, recall),
                  control = ctrl)
      
    }
    
    if(elnet_mode %in% "regression"){
      
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
    
    if ("glmn_tune" %in% output_objects) {
      output[["glmn_tune"]] <- glmn_tune
    }
    
    return(output)
  }
