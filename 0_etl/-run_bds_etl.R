# ----- Library Initiation ----- 
library(rvest)
library(stringr)
library(dplyr)
library(data.table)
library(xml2)
library(tidyverse)

rm(list = ls())
gc()

# ----- Set the Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ------ Sourcing the Scripts ------
source("0_etl/0_results_download.R")
source("0_etl/1_tipsport_feedxml.R")

rm(list = ls())
gc()