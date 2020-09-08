# ---- Library Load ----
library(dplyr)
library(data.table)
library(lightgbm)
library(tidyr)
library(purrr)
library(Matrix)
library(Rsolnp)
library(h2o)
library(lightgbm)
library(xgboost)

h2o.init()

# ----- set evaluation period -----
eval_period <- "2018-07-01"
b_fraction <- 0.2

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Models -----
m_lightgbm <- readRDS.lgb.Booster("2_ml_pipelines/db_temp/5_lightgbm_model.rds")
model_h2o <- 
  h2o.loadModel("2_ml_pipelines/db_temp/DeepLearning_model_R_1589285531075_2")
load("2_ml_pipelines/db_temp/5_xgboost_countmodel.RData")

# ----- Load Modelling Data -----
load("2_ml_pipelines/db_temp/modelling_data.RData")

# ----- Load Match Data -----
load("0_etl/db_temp/0_results_download.RData")

# ----- Load Budget and Sharpe Function -----
source("00_functions/sharpe_and_budget.R")
source("00_functions/get_result_data.R")
source("00_functions/get_backtesting_data.R")
source("00_functions/get_opt_data.R")
source("00_functions/get_poisson.R")
source("00_functions/get_prob.R")

# ----- Get Modelling Data - input for predict function -----
dfit <- 
  modelling_data %>%
  filter(match_date > eval_period) %>%
  distinct() %>%
  as.data.frame()

# ----- Determine the Round during the season -----
colnames_basic <- 
  c("league", "match_date", "is_home", "team", "match_result", "n_goals")
min_year <- min(lubridate::year(master_data$created_at))
max_year <- max(lubridate::year(master_data$created_at))
round_data <- data.frame(year = c(min_year:max_year))

rm(min_year)
rm(max_year)

round_data <- round_data %>%
  mutate(lag_year = lag(year)) %>%
  na.omit() %>%
  rowwise() %>%
  mutate(season = paste(lag_year, year, sep = "/")) %>%
  
  mutate(from_date = as.Date(paste(lag_year, "-07", "-01", sep = "")),
         to_date = as.Date(paste(year, "-07", "-31", sep = ""))) %>%
  dplyr::select(-year, -lag_year)
