# ----- Library Initiation ----- 
library(data.table)
library(dplyr)
library(tidyverse)
library(mice)

rm(list = ls())
gc()

# ----- Set the Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Results Data
load("0_etl/db_temp/0_results_download.RData")

# ----- Reorganization of Match Data -----
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
  # rowwise() %>%
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
  as_tibble() %>%
  
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
  
  # - De-Select unwanted columns
  select(-B365H, -B365D, -B365A, -BWH, -BWD, -BWA, 
         -IWH, -IWD, -IWA, -PSH, -PSD, -PSA, 
         -WHH, -WHD, -WHA, -VCH, -VCD, -VCA,
         -B365.2.5, -B365.2.5.1, -P.2.5, -P.2.5.1, -GB.2.5, -GB.2.5.1) %>%
  select(-FTR, -FTHG, -FTAG, -HS, -AS, -HST, -AST, -HF, -AF, -HC, -AC, 
         -HY, -AY, -HR, -AR) %>%
  as_tibble() %>%
  
  # - replace infinite values
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) %>%
  
  # - Replacing missing values with column mean
  mice(., m = 5, method = "mean", printFlag = T) %>%
  complete(.) %>%
  as_tibble()

gc()

# ----- Calculate predictors -----
for(j in unique(master_data$Div)){
  
  # j <- "E0"
  
  predictors_data <- 
    expand.grid("sport" = c("football"),
                "league" = j) %>%
                # "league" = master_data %>% pull(Div) %>% unique()) %>%
    mutate_if(., is.factor, as.character) %>%
    as_tibble() %>%
    
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
                   as_tibble() %>%
                   
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
                   as_tibble() %>%
                   
                   # - Nesting the data into another layer
                   group_by(match_date, is_home, team, gp_suffix) %>%
                   nest()
             )) %>%
    
    unnest(c(match_program)) %>%
    rename(hist_match = data)
  
  # ----- Destination Path -----
  dest_path <- "1_variable_calculator/db_temp/1_variable_calculator.RData"
  
  save("match_data", "predictors_data", 
       file = paste("1_variable_calculator/db_temp/1_variable_calculator_", j, 
                    ".RData", sep = ""))
}
