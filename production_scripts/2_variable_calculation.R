# ----- Library Initiation ----- 
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(broom)
library(yardstick)
library(doParallel)
library(foreach)

rm(list = ls())
gc()
which_sharpe <- "xml_data"

# ----- Set the Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
source("functions.R")

# ----- Load the Data -----
load("data/production_data/0_data_download.RData")

# ----- Compute Adjusted Variables -----
master_data <- master_data %>%
  rowwise() %>%
  mutate(adjusted_D = coalesce(B365D, BWD, IWD, PSD, WHD, VCD, LBD, SJD, GBD, 
                               BSD, SBD, SOD, SYD),
         adjusted_H = coalesce(B365H, BWH, IWH, PSH, WHH, VCH, LBH, SJH, GBH, 
                               BSH, SBH, SOH, SYH),
         adjusted_A = coalesce(B365A, BWA, IWA, PSA, WHA, VCA, LBA, SJA, GBA, 
                               BSA, SBA, SOA, SYA),
         adjusted_CD = coalesce(B365CD, BWCD, IWCD, PSCD, WHCD, VCCD),
         adjusted_CH = coalesce(B365CH, BWCH, IWCH, PSCH, WHCH, VCCH),
         adjusted_CA = coalesce(B365CA, BWCA, IWCA, PSCA, WHCA, VCCA),
         adjusted_251 = coalesce(B365.2.5.1, P.2.5.1, B365C.2.5.1, PC.2.5.1, 
                                 GB.2.5.1, Max.2.5.1, Avg.2.5.1),
         adjusted_250 = coalesce(B365.2.5, P.2.5, B365C.2.5, PC.2.5, GB.2.5, 
                                 Max.2.5, Avg.2.5),
         adjusted_C251 = coalesce(B365C.2.5.1, PC.2.5.1, B365C.2.5.1, PC.2.5.1, 
                                  MaxC.2.5.1, AvgC.2.5.1),
         adjusted_C250 = coalesce(B365C.2.5, PC.2.5, B365C.2.5, PC.2.5, 
                                  MaxC.2.5, AvgC.2.5),
         
         adjusted_meanD = mean(c(B365D, BWD, IWD, PSD, WHD, VCD, LBD, SJD, GBD, 
                                 BSD, SBD, SOD, SYD), na.rm = TRUE),
         adjusted_meanH = mean(c(B365H, BWH, IWH, PSH, WHH, VCH, LBH, SJH, GBH, 
                                 BSH, SBH, SOH, SYH), na.rm = TRUE),
         adjusted_meanA = mean(c(B365A, BWA, IWA, PSA, WHA, VCA, LBA, SJA, GBA, 
                                 BSA, SBA, SOA, SYA), na.rm = TRUE),
         adjusted_meanCD = mean(c(B365CD, BWCD, IWCD, PSCD, WHCD, VCCD), 
                                na.rm = TRUE),
         adjusted_meanCH = mean(c(B365CH, BWCH, IWCH, PSCH, WHCH, VCCH), 
                                na.rm = TRUE),
         adjusted_meanCA = mean(c(B365CA, BWCA, IWCA, PSCA, WHCA, VCCA), 
                                na.rm = TRUE),
         adjusted_mean251 = mean(c(B365.2.5.1, P.2.5.1, B365C.2.5.1, PC.2.5.1, 
                                   GB.2.5.1, Max.2.5.1, Avg.2.5.1)),
         adjusted_mean250 = mean(c(B365.2.5, P.2.5, B365C.2.5, PC.2.5, GB.2.5, 
                                   Max.2.5, Avg.2.5), na.rm = TRUE),
         adjusted_meanC251 = mean(c(B365C.2.5.1, PC.2.5.1, B365C.2.5.1, 
                                    PC.2.5.1, MaxC.2.5.1, AvgC.2.5.1), 
                                  na.rm = TRUE),
         adjusted_meanC250 = mean(c(B365C.2.5, PC.2.5, B365C.2.5, PC.2.5, 
                                    MaxC.2.5, AvgC.2.5), na.rm = TRUE),
         
         HomeTeam = trimws(HomeTeam, which = "both"),
         AwayTeam = trimws(AwayTeam, which = "both"),
         
         total_goals = FTAG + FTHG) %>%
  as.data.frame() %>%
  group_by(.) %>%
  distinct() %>%
  # - necessary columns!
  filter(!(is.na(HS))) %>%
  filter(!(is.na(AS))) %>%
  filter(!(is.na(HST))) %>%
  filter(!(is.na(AST))) %>%
  filter(!(is.na(HF))) %>%
  filter(!(is.na(AF))) %>%
  filter(!(is.na(HC))) %>%
  filter(!(is.na(AC))) %>%
  filter(!(is.na(HY))) %>%
  filter(!(is.na(AY))) %>%
  filter(!(is.na(HR))) %>%
  filter(!(is.na(AR))) %>%
  filter(!(is.na(adjusted_D))) %>%
  filter(!(is.na(adjusted_H))) %>%
  filter(!(is.na(adjusted_A)))

# ------ Compute on Data -----
if("2_variable_calculation.RData" %in% list.files("data/production_data")){
  load("data/production_data/2_variable_calculation.RData")
  
  dates_all <- rbind(master_data %>%
                       select(created_at, HomeTeam) %>%
                       rename(team_input = HomeTeam) %>%
                       distinct() %>%
                       as.data.frame(),
                     master_data %>%
                       select(created_at, AwayTeam) %>%
                       rename(team_input = AwayTeam) %>%
                       distinct() %>%
                       as.data.frame()) %>%
    as.data.frame() %>%
    distinct() %>%
    left_join(., modelling_data %>% 
                select(team_input, created_at) %>% 
                mutate(in_sample = 1)) %>%
    filter(is.na(in_sample)) %>%
    select(-in_sample)
}else{
  dates_all <- rbind(master_data %>%
                       select(created_at, HomeTeam) %>%
                       rename(team_input = HomeTeam) %>%
                       distinct() %>%
                       as.data.frame(),
                     master_data %>%
                       select(created_at, AwayTeam) %>%
                       rename(team_input = AwayTeam) %>%
                       distinct() %>%
                       as.data.frame()) %>%
    as.data.frame() %>%
    distinct()
}

threshold <- 500000
if(nrow(dates_all) > 0){
  
  output_list <- list()
  
  if(nrow(dates_all) <= threshold){
    n_iter <- nrow(dates_all)
  }else{
    n_iter <- threshold
  }
  
  # - Parallel Version
  start_time <- Sys.time()
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  
  hist_data <- foreach(i = 1:n_iter, 
                       .combine = rbind, 
                       .packages = c("data.table", "dplyr", 
                                     "tidymodels", "tidyr", 
                                     "tidyverse")) %dopar% {
                                       
                                       master_subset <- master_data %>% 
                                         filter(HomeTeam %in% dates_all$team_input[i] | 
                                                  AwayTeam %in% dates_all$team_input[i])
                                       
                                       temp_result <- 
                                         lapply(c(seq.int(from = 10, to = 50, by = 5)), 
                                                function(x) 
                                                  get_n_matches_statistics(
                                                    data_input = master_subset, 
                                                    team_input = dates_all$team_input[i], 
                                                    match_date = dates_all$created_at[i], 
                                                    number_matches = x))
                                       
                                       temp_data <- bind_rows(temp_result) %>% 
                                         as.data.frame() %>%
                                         summarise_all(., max, na.rm = TRUE) %>% 
                                         as.data.frame()
                                       
                                       temp_data[mapply(is.infinite, temp_data)] <- NA
                                       temp_data$created_at <- rep(dates_all$created_at[i], 
                                                                   nrow(temp_data))
                                       temp_data$team_input <- rep(dates_all$team_input[i], 
                                                                   nrow(temp_data))
                                       
                                       return(temp_data)
                                       # print(i)
                                     }
  end_time <- Sys.time()
  stopCluster(cl)
  
  # - Parallel Version
  
  # ----- Save Data ----- #
  if("2_variable_calculation.RData" %in% list.files("data/production_data")){
    load("data/production_data/2_variable_calculation.RData")
    
    modelling_data <- rbind(modelling_data, hist_data) %>% as.data.frame()
    save(modelling_data, 
         file = "data/production_data/2_variable_calculation.RData")
    
  }else{
    modelling_data <- hist_data %>% as.data.frame()
    save(modelling_data, 
         file = "data/production_data/2_variable_calculation.RData")
  }
}
