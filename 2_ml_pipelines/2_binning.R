library(tidyverse)
library(tidymodels)
library(woeBinning)

# ----- Load Modelling Data -----
load("2_ml_pipelines/db_temp/modelling_data.RData")
data_split <- initial_split(modelling_data, prop = 3/4, strata = "target_goals")

# ----- Split to Test and Train -----
train_data <- training(data_split)
test_data <- testing(data_split)

# ----- Variables Binning -----
no_binning_vars <- c("match_result", "n_goals", "match_date")
binning_model <- 
  woe.binning(df = train_data %>% select(-one_of(no_binning_vars)), 
              target.var = "target_goals", 
              pred.var = names(train_data)[!(names(train_data) %in% 
                                               c("target_goals", 
                                                 no_binning_vars))])

binning_output <- map_df(woe.binning.table(binning_model), 
                         ~as.data.frame(.x), 
                         .id="variable") %>%
  mutate(variable = stringr::str_replace_all(string = variable, 
                                             pattern = "WOE Table for ", 
                                             replacement = "")) %>%
  as.data.frame() %>%
  group_by(variable) %>%
  mutate(total_iv = sum(as.numeric(IV))) %>%
  arrange(desc(total_iv))

# ----- Variable Selection -----
del_vars <- 
  unique(binning_output$variable)[41:length(unique(binning_output$variable))]

binning_output %>%
  select(variable, total_iv) %>%
  distinct() %>%
  arrange(desc(total_iv)) %>%
  as.data.frame() %>%
  top_n(40, total_iv)

modelling_data <- 
  modelling_data %>% 
  select(-one_of(del_vars))

train_woe <- 
  woe.binning.deploy(train_data, binning = binning_model, 
                     min.iv.total = 0.03, add.woe.or.dum.var = "woe") %>%
  select(match_date, team, is_home, n_goals, match_result, contains("woe")) %>%
  select(-one_of(paste("woe.", del_vars, ".binned", sep = "")))

test_woe <- 
  woe.binning.deploy(test_data, binning = binning_model, 
                     min.iv.total = 0.03, add.woe.or.dum.var = "woe") %>%
  select(match_date, team, is_home, n_goals, match_result, contains("woe")) %>%
  select(-one_of(paste("woe.", del_vars, ".binned", sep = "")))

rm(list = ls()[!(ls() %in% c("binning_model", "test_woe", 
                             "train_woe", "data_split"))])
gc()

# ----- Saving Object -----
save(binning_model, 
     file = "2_ml_pipelines/db_temp/binning_model/binning_model.RData")
save(train_woe, file = "2_ml_pipelines/db_temp/train_data.RData")
save(test_woe, file = "2_ml_pipelines/db_temp/test_data.RData")
save(data_split, file = "2_ml_pipelines/db_temp/split_object.RData")
