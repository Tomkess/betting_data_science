# ----- Modelling Data Preparation -----
library(data.table)
library(dplyr)
library(rsample)
library(parsnip)
library(recipes)
library(dials)
library(tidymodels)
library(furrr)
library(stringr)
library(fastDummies)
library(lubridate)

rm(list = ls())
gc()

setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
test_date <- "2018-06-01"

# ----- Get the Predicted Variable -----
load(file = "data/production_data/0_data_download.RData")
winloss_data <- master_data

# ----- Read Variables -----
load(file = "data/production_data/2_variable_calculation.RData")
master_data <- modelling_data
rm(modelling_data)

get_div <- winloss_data %>%
  select(HomeTeam, created_at, Div) %>%
  rename(team_input = HomeTeam) %>%
  group_by(.) %>%
  distinct() %>%
  rbind(., winloss_data %>%
          select(AwayTeam, created_at, Div) %>%
          rename(team_input = AwayTeam) %>%
          group_by(.) %>%
          distinct()) %>%
  group_by(.) %>%
  distinct() %>%
  as.data.frame()

teams_selected <- master_data %>%
  group_by(team_input) %>%
  summarise(total_matches = n()) %>%
  filter(total_matches >= 200) %>%
  # filter(str_detect(string = Div, pattern = "0") | 
  #          str_detect(string = Div, pattern = "1")) %>%
  # select(HomeTeam, AwayTeam) %>%
  # select(team_input) %>%
  distinct()

# unique_teams <- unique(c(teams_selected$HomeTeam, teams_selected$AwayTeam))
unique_teams <- unique(teams_selected$team_input)

winloss_data <- winloss_data %>%
  rowwise() %>%
  mutate(adjusted_D = coalesce(B365D, BWD, IWD, PSD, WHD, VCD, LBD, SJD, GBD, 
                               BSD, SBD, SOD, SYD),
         adjusted_H = coalesce(B365H, BWH, IWH, PSH, WHH, VCH, LBH, SJH, GBH, 
                               BSH, SBH, SOH, SYH),
         adjusted_A = coalesce(B365A, BWA, IWA, PSA, WHA, VCA, LBA, SJA, GBA, 
                               BSA, SBA, SOA, SYA),
         HomeTeam = trimws(HomeTeam, which = "both"),
         AwayTeam = trimws(AwayTeam, which = "both"),
         total_goals = FTAG + FTHG) %>%
  select(created_at, HomeTeam, AwayTeam, FTR, FTAG, FTHG, adjusted_H, 
         adjusted_A, adjusted_D) %>%
  rowwise() %>%
  mutate(total_goals = FTAG + FTHG,
         win_loss = ifelse(FTR %in% "H", 
                           HomeTeam, ifelse(FTR %in% "A", AwayTeam, "Draw")),
         winloss_indicator = 1)
winloss_data[is.na(winloss_data)] <- 1

# ----- Sampling the Data -----

# - draw result
winloss_data$samples_draw <- 1
winloss_data$samples_draw[winloss_data$win_loss %in% "Draw"] <- 
  ceiling(winloss_data$adjusted_D[winloss_data$win_loss %in% "Draw"]) + 1

# - total samples
winloss_data$total_samples <- winloss_data$samples_draw

master_data <- master_data %>%
  left_join(., winloss_data %>% 
              select(created_at, win_loss, winloss_indicator) %>% 
              as.data.frame() %>%
              distinct(), by = c("created_at" = "created_at", 
                                      "team_input" = "win_loss")) %>%
  left_join(., winloss_data %>% 
              select(created_at, HomeTeam, total_goals, total_samples) %>% 
              rename(team_input = HomeTeam) %>% 
              rbind(., winloss_data %>% 
                      select(created_at, AwayTeam, total_goals, 
                             total_samples) %>% 
                      rename(team_input = AwayTeam)) %>% 
              as.data.frame() %>%
              distinct(), by = c("created_at" = "created_at", 
                                 "team_input" = "team_input"))

master_data$winloss_indicator[is.na(master_data$winloss_indicator)] <- 0
master_data <- master_data %>% 
  rename(win_loss = winloss_indicator) %>%
  filter(team_input %in% unique_teams)

rm(winloss_data)
rm(teams_selected)

# ----- Winner Prediction Data -----
training_winner <- master_data %>% 
  filter(created_at <= as.Date(test_date)) %>%
  # mutate(i = row_number()) %>%
  # group_by(i) %>%
  # do(sample_n(., total_samples, replace = TRUE)) %>%
  as.data.frame() %>%
  select(-total_samples) %>%
  mutate(win_loss_factor = ifelse(win_loss == 0, "L", "W")) %>%
  select(-win_loss, -total_goals) %>%
  rename(win_loss = win_loss_factor) %>%
  as.data.frame() %>%
  dummy_cols(., select_columns = "team_input") %>%
  left_join(., get_div) %>%
  as.data.frame() %>%
  mutate(year = year(created_at)) %>%
  dummy_cols(., select_columns = "Div") %>%
  select(-team_input, -created_at, -Div) %>%
  as.data.frame()

recipe_winner <- recipe(training_winner, formula = win_loss ~ .,)
recipe_winner <- recipe_winner %>%
  step_meanimpute(all_predictors()) %>%
  step_range(min = 0, max = 1, all_predictors()) %>%
  prep(retain = TRUE)

prepared_winner <- recipe_winner %>%
  prep(retain = TRUE) %>%
  bake(new_data = training_winner) %>%
  as.data.frame()

model_recipe_winner <- recipe(prepared_winner, formula = win_loss ~ .,)

rm(training_winner)
rm(testing_winner)
gc()

# ----- Saving the Output -----
rm(master_data)
save.image("data/production_data/3_modelling_data.RData")
