# ----- Load Library -----
library(dplyr)
library(data.table)
library(config)

# ----- Set the Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")
var_files <- data.frame("files" = list.files("1_variable_calculator/db_temp"))
var_files$league_name <- 
  stringr::str_remove_all(stringr::str_remove_all(var_files$files, pattern = ".RData"), 
                          "1_variable_calculator_")

# ----- Load Predictors Function -----
source("2_ml_pipelines/0_predictors_function.R")
gc()

# ----- Compute Modelling Data -----
for(j in var_files$league_name){
  # j <- "E0"
  
  # - get already computed leagues
  already_computed <- list.files("2_ml_pipelines/db_temp")
  already_computed <- 
    already_computed[stringr::str_detect(already_computed, 
                                         pattern = ".RData") == TRUE]
  already_computed <- 
    already_computed[stringr::str_detect(already_computed, 
                                         pattern = "_data_") == TRUE]
  already_computed <- 
    stringr::str_remove_all(stringr::str_remove_all(already_computed, 
                                                    pattern = ".RData"), "modelling_data_")
  
  print(j)
  if(!(j %in% already_computed)){
    # ----- Load Team Mapping -----
    load(paste("1_variable_calculator/db_temp/1_variable_calculator_", j, ".RData", sep = ""))
    
    # j <- "EC"
    # - Compute modelling data for selected league
    modelling_temp <- 
      get_modellingdata(predictors_data %>%
                          filter(league %in% j)) %>%
      left_join(., match_data %>% 
                  select(created_at, is_home, team, match_result, n_goals) %>%
                  as.data.frame(), 
                by = c("match_date" = "created_at", 
                       "team" = "team", 
                       "is_home" = "is_home")) %>%
      select(-sport) %>%
      as.data.frame()
    
    rm(predictors_data)
    gc()
    
    # - save immediately in folder
    save(modelling_temp, 
         file = paste("2_ml_pipelines/db_temp/modelling_data_", 
                      j, ".RData", sep = ""))
  }
  
  print(j)
}

# - get already computed leagues
already_computed <- list.files("2_ml_pipelines/db_temp")
already_computed <- 
  already_computed[stringr::str_detect(already_computed, 
                                       pattern = ".RData") == TRUE]
already_computed <- 
  already_computed[stringr::str_detect(already_computed, 
                                       pattern = "_data_") == TRUE]
already_computed <- 
  stringr::str_remove_all(stringr::str_remove_all(already_computed, 
                                                  pattern = ".RData"), "modelling_data_")

o_modelling_data <- list()
for(i in already_computed){
  load(paste("2_ml_pipelines/db_temp/modelling_data_", i, ".RData", sep = ""))
  
  o_modelling_data[[i]] <- modelling_temp
}

modelling_data <- o_modelling_data %>% bind_rows()
rm(list = ls()[!(ls() %in% "modelling_data")])
gc()

save(modelling_data, 
     file = "2_ml_pipelines/db_temp/modelling_data.RData")
