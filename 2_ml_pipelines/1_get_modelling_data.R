# ----- Load Library -----
library(dplyr)
library(data.table)
library(config)

# ----- Load Team Mapping -----
dest_path <- "1_variable_calculator/db_temp/1_variable_calculator.RData"
load(paste(get("working_directory")[["4finance_PC"]], 
           "/", dest_path, sep = ""))

# ----- Load Predictors Function -----
source(paste(get("working_directory")[["4finance_PC"]], 
             "/2_ml_pipelines/0_predictors_function.R", sep = ""))

# ----- Compute Modelling Data -----
output_modelling <- list()
count <- 1
for(j in unique(predictors_data$league)){
  # j <- "E0"
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
  
  if(count == 1){
    modelling_data <- modelling_temp %>% as.data.frame()
    count <- count + 1
  }else{
    modelling_data <- 
      modelling_data %>%
      rbind(., modelling_temp) %>%
      as.data.frame()
    
    count <- count + 1
  }
  
  # - save immediately in folder
  save(modelling_data, file = "2_ml_pipelines/db_temp/modelling_data.RData")
}
