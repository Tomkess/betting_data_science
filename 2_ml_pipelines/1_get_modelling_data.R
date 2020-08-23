# ----- Load Library -----
library(dplyr)
library(data.table)
library(purrr)
library(stringr)
library(tidyr)

# ----- Set the Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")
var_files <- 
  data.frame("files" = list.files("1_variable_calculator/db_temp")) %>%
  mutate(league = str_remove_all(files, ".RData")) %>%
  mutate(league_name = str_remove_all(league, "1_variable_calculator_")) %>%
  mutate(files_path = paste("1_variable_calculator/db_temp/", files, sep = ""))

# ----- Load Predictors Function -----
source("2_ml_pipelines/0_predictors_function.R")
gc()

# ----- Compute Modeling Data -----
for (j in 1:nrow(var_files)) {
  # j <- 1
  
  out_temp <- list()
  
  s_league <- var_files$league_name[j]
  n_batch <- 20
  count <- 1
  
  # - Load predictors_data
  load(var_files$files_path[j])
  
  # - split the league teams into N groups
  n_teams <- length(unique(predictors_data$team))
  temp_df <- data.frame("team" = unique(predictors_data$team),
                        "group" = ceiling(c(1:n_teams)/n_batch))
  
  # - run computation for each group
  for(k in unique(temp_df$group)){
    # k <- 1
    
    s_teams_u <- temp_df$team[temp_df$group == k]
    
    # - Compute modeling data for selected league
    out_temp[[count]] <- 
      get_modellingdata(predictors_data %>% 
                          filter(league %in% s_league & 
                                   team %in% s_teams_u)) %>%
      left_join(., match_data %>%
                  filter(team %in% s_teams_u) %>%
                  select(created_at, is_home, team, match_result, n_goals) %>%
                  as.data.frame(), 
                by = c("match_date" = "created_at", 
                       "team" = "team", 
                       "is_home" = "is_home")) %>%
      select(-sport) %>%
      as.data.frame()
    
    count <- count + 1
  }
  
  # - saves the file
  modelling_temp <- out_temp %>% bind_rows() %>% as.data.frame()
  save(modelling_temp, 
       file = paste("2_ml_pipelines/db_temp/modelling_data/modelling_data_", 
                    s_league, ".RData", sep = ""))
  
  print(s_league)
}

# ----- Get Modeling Data -----
modelling_data <- 
  
  # - list all files
  data.frame(files = list.files("2_ml_pipelines/db_temp/modelling_data")) %>% 
  
  # - finish path
  mutate(files_path = 
           paste("2_ml_pipelines/db_temp/modelling_data/", files, sep = "")) %>%

  # - load one by one
  group_by(files_path) %>%
  mutate(data = map(files_path, 
                    function(i_files_path){
                      load(i_files_path)
                      return(modelling_temp)})) %>%
  unnest(c(data)) %>%
  as.data.frame() %>%
  select(-files, -files_path) %>%
  
  # - define target
  mutate(target_goals = ifelse(n_goals >= 2.5, "Over 2.5", "Under 2.5")) %>%
  mutate_if(is.character, as.factor)

rm(list = ls()[!(ls() %in% c("modelling_data"))])
gc()

# ----- Saving Object -----
save(modelling_data, file = "2_ml_pipelines/db_temp/modelling_data.RData")
