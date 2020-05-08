# ----- Library Initiation ----- 
library(data.table)
library(dplyr)
library(tidyverse)
library(mice)

rm(list = ls())
gc()

# ----- Destination Path -----
dest_path <- "1_variable_calculator/db_temp/1_variable_calculator.RData"

# ----- Set the Working Directory -----
if(Sys.info()[['nodename']] %in% c("966916-dci1-adw-002.ofg.local")){
  # - path on server
  setwd("/home/peter.tomko/concept---data-science")
}

if(Sys.info()[['nodename']] %in% c("CZIT-NTB-017")){
  # - path on 4finance ntb
  setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
}

if(Sys.info()[['nodename']] %in% c("PETER")){
  # - path on private ntb
  setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")
}

# ----- Run A Script -----
source("1_variable_calculator/0_variable_calculation.R")
