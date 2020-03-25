# ----- Library Initiation ----- 
library(data.table)
library(dplyr)
library(tidyverse)
library(mice)

rm(list = ls())
gc()
# which_sharpe <- "xml_data"

# ----- Set the Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
load("data/production_data/0_data_download.RData")

# ----- Reorganziation of Match Data -----
match_data <- 
  master_data %>%
  select(created_at, HomeTeam, AwayTeam, FTR, 
         FTHG, FTAG, HS, AS, HST, AST, HF, AF, HC, AC, 
         HY, AY, HR, AR,
         B365H, B365D, B365A, BWH, BWD, BWA, 
         IWH, IWD, IWA, PSH, PSD, PSA, 
         WHH, WHD, WHA, VCH, VCD, VCA,
         B365.2.5, B365.2.5.1, P.2.5, P.2.5.1, GB.2.5, GB.2.5.1) %>%
  distinct() %>%
  gather(., is_home, team, 
         -created_at, -FTR,
         # - Match Statistics
         -FTHG, -FTAG, -HS, -AS, -HST, -AST, -HF, -AF, -HC, -AC, 
         -HY, -AY, -HR, -AR,
         # - Bet Odds
         -B365H, -B365D, -B365A, -BWH, -BWD, -BWA, 
         -IWH, -IWD, -IWA, -PSH, -PSD, -PSA, 
         -WHH, -WHD, -WHA, -VCH, -VCD, -VCA,
         -B365.2.5, -B365.2.5.1, -P.2.5, -P.2.5.1, -GB.2.5, -GB.2.5.1) %>%
  mutate(is_home = recode(is_home, "HomeTeam" = 1, "AwayTeam" = 0)) %>%
  
  # - Create match result
  mutate(match_result = case_when(FTR == "H" & is_home == 1 ~ 1, # home wins
                                  FTR == "A" & is_home == 0 ~ 0, # away wins
                                  FTR == "A" & is_home == 1 ~ 0, # away loses
                                  FTR == "H" & is_home == 0 ~ 0, # home loses
                                  FTR == "D" ~ -1)) %>%
  
  # - get number of goals
  mutate(n_goals = case_when(is_home == 1 ~ FTHG, is_home == 0 ~ FTAG),
         n_shots = case_when(is_home == 1 ~ HS, is_home == 0 ~ AS),
         n_shots_ontarget = case_when(is_home == 1 ~ HST, is_home == 0 ~ AST),
         n_fauls = case_when(is_home == 1 ~ HF, is_home == 0 ~ AF),
         n_corners = case_when(is_home == 1 ~ HC, is_home == 0 ~ AC),
         n_yellow_cards = case_when(is_home == 1 ~ HY, is_home == 0 ~ AY),
         n_red_cards = case_when(is_home == 1 ~ HR, is_home == 0 ~ AR)) %>%
  select(-FTR, -FTHG, -FTAG, -HS, -AS, -HST, -AST, -HF, -AF, -HC, -AC, 
         -HY, -AY, -HR, -AR) %>%
  
  # - Create derived variables
  mutate(r_shots_goals = n_shots/(n_goals + 1),
         r_shotsontarget_goals = n_shots_ontarget/(n_goals + 1),
         r_corners_goals = n_corners/(n_goals + 1),
         r_shotsontarget_shots = n_shots_ontarget/(n_shots + 1),
         
         r_bookmakers_fee = 
           coalesce(100 - (100/B365A + 100/B365H + 100/B365D),
                    100 - (100/BWA + 100/BWH + 100/BWD),
                    100 - (100/IWA + 100/IWH + 100/IWD),
                    100 - (100/PSA + 100/PSH + 100/PSD),
                    100 - (100/WHA + 100/WHH + 100/WHD),
                    100 - (100/VCA + 100/VCH + 100/VCD)),
         
         r_team_odds = 
           case_when(is_home == 1 ~ coalesce(B365H, BWH, IWH, PSH, WHH, VCH), 
                     is_home == 0 ~ coalesce(B365A, BWA, IWA, PSA, WHA, VCA)),
         r_draw_odds = coalesce(B365D, BWD, IWD, PSD, WHD, VCD),
         r_team_overfair = abs(0.33 - 1/r_team_odds),
         
         r_over_25 = coalesce(B365.2.5.1, P.2.5.1, GB.2.5.1),
         r_under_25 = coalesce(B365.2.5, P.2.5, GB.2.5),
         
         r_overfair_25 = abs(0.5 - 1/coalesce(B365.2.5.1, P.2.5.1, GB.2.5.1)),
         r_overfair_25 = abs(0.5 - 1/coalesce(B365.2.5, P.2.5, GB.2.5))) %>%
  as.data.frame() %>%
  
  # - Create strength indices
  group_by(team) %>%
  arrange(created_at) %>%
  mutate(n_overall_strength = 100 + cumsum(match_result),
         n_overall_strength_h = 100 + cumsum(match_result * is_home),
         n_overall_strength_a = 100 + cumsum(match_result * abs(is_home - 1)),
         
         n_overall_strength_w = 100 + cumsum(match_result/(n_goals + 1)),
         n_overall_strength_wh = 
           100 + cumsum(match_result * is_home/(n_goals + 1)),
         n_overall_strength_wa = 
           100 + cumsum(match_result * abs(is_home - 1)/(n_goals + 1))) %>%
  
  select(-B365H, -B365D, -B365A, -BWH, -BWD, -BWA, 
         -IWH, -IWD, -IWA, -PSH, -PSD, -PSA, 
         -WHH, -WHD, -WHA, -VCH, -VCD, -VCA,
         -B365.2.5, -B365.2.5.1, -P.2.5, -P.2.5.1, -GB.2.5, -GB.2.5.1) %>%
  
  # - Replacing missing values with column mean
  mice(., m = 5, method = "mean", printFlag = T) %>%
  complete(.)

# ----- Calculate predictors -----
test <- 
  expand.grid("sport" = c("football"),
              "league" = master_data %>% pull(Div) %>% unique()) %>%
  mutate_if(., is.factor, as.character) %>%
  as.data.frame() %>%
  
  group_by(sport, league) %>%
  mutate("match_program" = 
           map(league, 
               function(league_i)
                 
                 # - get list of matches
                 master_data %>%
                 filter(Div %in% league_i) %>%
                 select(HomeTeam, AwayTeam, created_at) %>%
                 gather(., is_home, team, -created_at) %>%
                 mutate(is_home = recode(is_home, 
                                         "HomeTeam" = 1, 
                                         "AwayTeam" = 0)) %>%
                 distinct() %>%
                 rename(match_date = created_at) %>%
                 
                 # - Join all matches to specific team
                 left_join(., master_data %>%
                             filter(Div %in% league_i) %>%
                             select(HomeTeam, AwayTeam, created_at) %>%
                             gather(., is_home, team, -created_at) %>%
                             select(-is_home) %>%
                             distinct() %>%
                             rename(prev_match_date = created_at)) %>%
                 
                 # - Subset historical, i.e. created_at >= prev_match_date
                 filter(match_date > prev_match_date) %>%
                 
                 # - Pick only 50 dates
                 group_by(match_date, team) %>%
                 arrange(desc(prev_match_date)) %>%
                 top_n(50, prev_match_date) %>%
                 mutate(last_n = row_number()) %>%
                 as.data.frame() %>%
                 
                 # - Sort matches into groups(last_5, last_10 etc.)
                 left_join(., expand.grid("last_n" = c(1:50),
                                          "group" = c(1:10) * 5,
                                          "suffix" = "last") %>%
                             filter(last_n <= group) %>%
                             rowwise() %>%
                             mutate(gp_suffix = 
                                      paste(suffix, "_", group, sep = "")) %>%
                             select(-suffix, -group) %>%
                             as.data.frame(), by = "last_n") %>%
                 
                 # - Nesting the data into another layer
                 group_by(match_date, is_home, team, gp_suffix) %>%
                 nest()
           )) %>%
  
  unnest(c(match_program)) %>%
  rename(hist_match = data)

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
