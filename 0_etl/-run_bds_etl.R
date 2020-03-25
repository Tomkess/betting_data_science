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
  setwd("C:/Users/Peter/Desktop/ds_project/betting_data_science")
}

# ------ Sourcing the Scripts ------
source("0_etl/0_results_download.R")
source("0_etl/1_tipsport_feedxml.R")

rm(list = ls())
gc()
