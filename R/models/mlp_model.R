mlp_model <- 
  
  function(traind_df, n_folds, final_metric,
           mlp_mode = "regression", strata_var, target_var,
           output_objects = c("parameters", "final_model", "glmn_tune")){
    
    # ----- Create Formula -----
    get_formula <- as.formula(paste(target_var, " ~ .", sep = ""))
    
    # ----- Model Structure -----
    ml_model_str <-
      
      mlp(hidden_units = tune(), dropout = tune(), 
          activation = "relu", epochs = 5) %>%
      
      set_engine("keras") %>% 
      set_mode(mlp_mode) %>%
      translate()
    
    # ----- Stratified Sampling -----
    cv_splits <- vfold_cv(train_df, v = n_folds, strata = strata_var)
    
    # ----- Create Workflow -----
    ml_workflow <- 
      workflow() %>%
      add_model(ml_model_str) %>%
      add_formula(get_formula)
    
    # ----- Set Parameters -----
    glmn_set <- parameters(hidden_units(),
                           dropout())
    
    # ----- Create Grid -----
    glmn_grid <- grid_max_entropy(glmn_set, size = 100)
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