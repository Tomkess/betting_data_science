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

# ----- Set the Working Directory -----
if(Sys.info()[['nodename']] %in% c('966916-dci1-adw-002.ofg.local')){
  # - path on server
  setwd("/home/peter.tomko/concept---data-science")
}else{
  # - path on local
  setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
}

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

# ----- Create Variable Function -----
get_n_matches_statistics <- function(data_input, team_input, 
                                     match_date, number_matches){
  
  # data_input = master_data
  # team_input = "Liverpool"
  # match_date = "2019-08-09"
  # number_matches = 10
  
  stats_data <- data_input %>%
    filter(created_at < match_date & 
             (HomeTeam %in% team_input | AwayTeam %in% team_input)) %>%
    arrange(desc(created_at)) %>%
    as.data.frame()
  
  if(nrow(stats_data) <= number_matches){
    stats_data <- stats_data
  }else{
    stats_data <- stats_data[1:number_matches,]
  }
  
  # ----- Get Historical Variables ----- 
  historical_stats <- stats_data %>%
    group_by(.) %>%
    summarise(total_goals = 
                sum(c(FTHG[HomeTeam %in% team_input],
                      FTAG[AwayTeam %in% team_input]), na.rm = TRUE),
              total_obtained = 
                sum(c(FTHG[!(HomeTeam %in% team_input)], 
                      FTAG[!(AwayTeam %in% team_input)]), na.rm = TRUE),
              home_matches = 
                length(HomeTeam[HomeTeam %in% team_input])/n(),
              away_matches = 
                length(AwayTeam[AwayTeam %in% team_input])/n(),
              
              # - sum mean sd of number of shots per match
              sum_shots = sum(HS + AS),
              mean_shots = mean(HS + AS),
              sd_shots = sd(HS + AS),
              
              sum_shots_teaminput = 
                sum(c(HS[HomeTeam %in% team_input],
                      AS[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_shots_teaminput = 
                mean(c(HS[HomeTeam %in% team_input], 
                       AS[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_shots_teaminput = 
                sd(c(HS[HomeTeam %in% team_input], 
                     AS[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - mean number of shots on target per match
              sum_shotsontarget = sum(HST + AST),
              mean_shotsontarget = mean(HST + AST),
              sd_shotsontarget = sd(HST + AST),
              
              sum_shotsontarget_teaminput = 
                sum(c(HST[HomeTeam %in% team_input], 
                      AST[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_shotsontarget_teaminput = 
                mean(c(HST[HomeTeam %in% team_input], 
                       AST[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_shotsontarget_teaminput = 
                sd(c(HST[HomeTeam %in% team_input], 
                     AST[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - mean number of fauls per match
              sum_fauls = sum(HF + AF),
              mean_fauls = mean(HF + AF),
              sd_fauls = sd(HF + AF),
              
              sum_fauls_teaminput = 
                sum(c(HF[HomeTeam %in% team_input],
                      AF[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_fauls_teaminput = 
                mean(c(HF[HomeTeam %in% team_input], 
                       AF[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_fauls_teaminput = 
                sd(c(HF[HomeTeam %in% team_input],
                     AF[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - mean number of corners per match
              sum_corners = sum(HC + AC),
              mean_corners = mean(HC + AC),
              sd_corners = sd(HC + AC),
              
              sum_corners_teaminput = 
                sum(c(HC[HomeTeam %in% team_input], 
                      AC[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_corners_teaminput = 
                mean(c(HC[HomeTeam %in% team_input], 
                       AC[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_corners_teaminput = 
                sd(c(HC[HomeTeam %in% team_input], 
                     AC[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - cumulative number of yellow cards in matches
              sum_yellow = sum(HY + AY),
              mean_yellow = mean(HY + AY),
              sd_yellow = sd(HY + AY),
              
              sum_yellow_teaminput = 
                sum(c(HY[HomeTeam %in% team_input],
                      AY[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_yellow_teaminput = 
                mean(c(HY[HomeTeam %in% team_input],
                       AY[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_yellow_teaminput = 
                sd(c(HY[HomeTeam %in% team_input], 
                     AY[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - cumulative number of red cards
              sum_red = sum(HR + AR),
              mean_red = mean(HR + AR),
              sd_red = sd(HR + AR),
              
              sum_red_teaminput = 
                sum(c(HR[HomeTeam %in% team_input],
                      AR[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_red_teaminput = 
                mean(c(HR[HomeTeam %in% team_input],
                       AR[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_red_teaminput = 
                sd(c(HR[HomeTeam %in% team_input],
                     AR[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - the strength index - the rate of non lost games (win + draws)/matches played
              strength_index = 
                (length(created_at[HomeTeam %in% team_input & FTR %in% "H"]) +
                   length(created_at[AwayTeam %in% team_input & FTR %in% "A"]) +
                   length(created_at[HomeTeam %in% team_input & FTR %in% "D"]) +
                   length(created_at[AwayTeam %in% team_input & FTR %in% "D"]))/n(),
              
              # - shots success rate, i.e. the rate of shots on target, of team_input - mean and sd
              mean_shotsrate_teaminput = 
                mean(c(HST[HomeTeam %in% team_input]/(1 + HS[HomeTeam %in% team_input]),
                       AST[AwayTeam %in% team_input]/(1 + AS[AwayTeam %in% team_input]))),
              
              sd_shotsrate_teaminput = 
                sd(c(HST[HomeTeam %in% team_input]/(1 + HS[HomeTeam %in% team_input]),
                     AST[AwayTeam %in% team_input]/(1 + AS[AwayTeam %in% team_input])))
              ) %>%
    rowwise() %>%
    mutate(rate_scored = total_goals/(total_goals + total_obtained),
           
           # - share variables
           ratio_sum_shots = sum_shots_teaminput/sum_shots,
           ratio_mean_shots = mean_shots_teaminput/mean_shots,
           ratio_sd_shots = sd_shots_teaminput/sd_shots,
           ratio_sum_shotsontarget = sum_shotsontarget_teaminput/sum_shotsontarget,
           ratio_mean_shotsontarget = mean_shotsontarget_teaminput/mean_shotsontarget,
           ratio_sd_shotsontarget = sd_shotsontarget_teaminput/sd_shotsontarget,
           ratio_sum_fauls = sum_fauls_teaminput/sum_fauls,
           ratio_mean_fauls = mean_fauls_teaminput/mean_fauls,
           ratio_sd_fauls = sd_fauls_teaminput/sd_fauls,
           ratio_sum_corners = sum_corners_teaminput/sum_corners,
           ratio_mean_corners = mean_corners_teaminput/mean_corners,
           ratio_sd_corners = sd_corners_teaminput/sd_corners,
           ratio_sum_yellow = sum_yellow_teaminput/sum_yellow,
           ratio_mean_yellow = mean_yellow_teaminput/mean_yellow,
           ratio_sd_yellow = sd_yellow_teaminput/sd_yellow,
           ratio_sum_red = sum_red_teaminput/sum_red,
           ratio_mean_red = mean_red_teaminput/mean_red,
           ratio_sd_red = sd_red_teaminput/sd_red,
    ) %>%
    as.data.frame()
  
  # - slope of regression
  if(nrow(stats_data) >= 10){
    
    beta_slope <- stats_data %>% 
      select(created_at, HomeTeam, AwayTeam, FTR, FTHG, FTAG) %>%
      arrange(created_at) %>%
      mutate(index = row_number(),
             index_sq = row_number() * row_number()) %>%
      mutate(scored_goals = cumsum(replace(FTAG, !(AwayTeam %in% team_input), 0) + replace(FTHG, !(HomeTeam %in% team_input), 0))) %>%
      do(tidy(lm(scored_goals ~ index + index_sq, data = .))) %>%
      filter(term %in% "index") %>%
      select(estimate)
    
    beta_slope <- as.numeric(beta_slope$estimate)
    
  }else{
    beta_slope <- NA
  }
  
  historical_stats$beta_slope <- beta_slope
  # ----- Get Historical Variables ----- #
  
  names(historical_stats) <- paste(names(historical_stats), "_", number_matches, sep = "")
  
  return(historical_stats)
}

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
