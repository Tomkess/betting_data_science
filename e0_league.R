# ----- Library Initiation ----- 
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
library(broom)
library(yardstick)
library(doParallel)
library(foreach)
library(fastDummies)
library(xgboost)
library(Rsolnp)

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

xml_data$HomeGoals <- rep(NA, nrow(xml_data))
xml_data$AwayGoals <- rep(NA, nrow(xml_data))

for(i in 1:nrow(xml_data)){
  print(i)
  # i <- 1
  
  temp_result_home <- 
    lapply(c(seq.int(from = 10, to = 50, by = 5)), 
           function(x) 
             get_n_matches_statistics(
               data_input = master_data, 
               team_input = xml_data$HomeTeam_tipsport[i], 
               match_date = xml_data$dateclosed[i], 
               number_matches = x)) %>%
    dplyr::bind_rows() %>%
    as.data.frame() %>%
    dplyr::summarise_all(., max, na.rm = TRUE) %>% 
    as.data.frame() %>%
    dplyr::mutate(created_at = xml_data$dateclosed[i],
                  team_input = xml_data$HomeTeam_tipsport[i]) %>%
    dplyr::mutate(year = lubridate::year(created_at),
                  month = lubridate::month(created_at),
                  quarter = lubridate::quarter(created_at),
                  week = lubridate::week(created_at),
                  home = 1) %>%
    as.data.frame() %>%
    dummy_cols(., select_columns = "team_input") %>%
    dplyr::mutate(Div = "E0") %>%
    dummy_cols(., select_columns = "Div") %>%
    as.data.frame()
  temp_result_home[mapply(is.infinite, temp_result_home)] <- NA
  
  temp_result_away <- 
    lapply(c(seq.int(from = 10, to = 50, by = 5)), 
           function(x) 
             get_n_matches_statistics(
               data_input = master_data, 
               team_input = xml_data$AwayTeam_tipsport[i], 
               match_date = xml_data$dateclosed[i], 
               number_matches = x)) %>%
    dplyr::bind_rows() %>%
    as.data.frame() %>%
    dplyr::summarise_all(., max, na.rm = TRUE) %>% 
    as.data.frame() %>%
    dplyr::mutate(created_at = xml_data$dateclosed[i],
                  team_input = xml_data$AwayTeam_tipsport[i]) %>%
    dplyr::mutate(year = lubridate::year(created_at),
                  month = lubridate::month(created_at),
                  quarter = lubridate::quarter(created_at),
                  week = lubridate::week(created_at),
                  home = 0) %>%
    as.data.frame() %>%
    dummy_cols(., select_columns = "team_input") %>%
    dplyr::mutate(Div = "E0") %>%
    dummy_cols(., select_columns = "Div") %>%
    as.data.frame()
  temp_result_away[mapply(is.infinite, temp_result_away)] <- NA
  
  temp_data <- plyr::rbind.fill(temp_result_away, temp_result_home)
  temp_data[is.na(temp_data)] <- 0
  
  names(temp_data) <- str_replace_all(string = names(temp_data), 
                                      pattern = " ", 
                                      replacement = "__")
  
  names(temp_data) <- str_replace_all(string = names(temp_data), 
                                      pattern = "'", 
                                      replacement = "")
  
  names(temp_data) <- str_replace_all(string = names(temp_data), 
                                      pattern = "__&", 
                                      replacement = "")
  
  names(temp_data) <- str_replace_all(string = names(temp_data), 
                                      pattern = "-", 
                                      replacement = "_")
  
  other_dims <- xgboost_model_output$E0$feature_names[!(xgboost_model_output$E0$feature_names %in% names(temp_data))]
  additional_vars <- as.data.frame(matrix(0, ncol = length(other_dims), nrow = 1))
  names(additional_vars) <- other_dims
  
  temp_data <- cbind(temp_data, additional_vars)
  temp_data$goals <- 0
  temp_data$Div <- "E0"
  
  temp_pred <- get_predictions(data = temp_data, model_list = xgboost_model_output)
  xml_data$HomeGoals[i] <- temp_pred[2]
  xml_data$AwayGoals[i] <- temp_pred[1]
}

output_prob <- list()
for(i in 1:nrow(xml_data)){
  output_prob[[i]] <- get_probabilities(xml_data[i,])
}

xml_data <- xml_data %>%
  cbind(., do.call("rbind", output_prob) %>% as.data.frame())

xml_data <- xml_data %>%
  rowwise() %>%
  mutate(prob_goals = max(goals_25, goals_251),
         goals_odds = ifelse(goals_25 > goals_251, 
                             `Méně než 2.5`, `Více než 2.5`))

# ---- Get Optimized Bet -----
opt_goals <- solnp(pars = c(rep(0.01, nrow(xml_data))), 
                   fun = sharpe_goals, 
                   eqfun = allocation_budget, 
                   eqB = 0.1,
                   LB = c(rep(0.0, nrow(xml_data))),
                   UB = c(rep(1, nrow(xml_data))))

xml_data$bet_fraction <- opt_goals$pars * 100000

library(xlsx)
write.xlsx(x = xml_data %>% as.data.frame(), file = "next_PremierLeague.xlsx", 
           row.names = F, sheetName = "Bet")
