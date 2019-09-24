# ----- Modelling Dat Preparation -----

library(data.table)
library(dplyr)
library(rsample)
library(parsnip)
library(recipes)
library(dials)
library(tidymodels)
library(furrr)

test_date <- "2018-10-05"

# ----- Read Variables -----
master_data <- 
  readRDS(file = "data/production_data/2_variable_calculation.RData")

# ----- Get the Predicted Variable -----
winloss_data <- readRDS(file = "data/production_data/0_data_download.RData")
winloss_data <- winloss_data %>%
  select(created_at, HomeTeam, AwayTeam, FTR, FTAG, FTHG) %>%
  rowwise() %>%
  mutate(total_goals = FTAG + FTHG,
         win_loss = ifelse(FTR %in% "H", 
                           HomeTeam, ifelse(FTR %in% "A", AwayTeam, "Draw")),
         winloss_indicator = 1)

master_data <- master_data %>%
  left_join(., winloss_data %>% 
              select(created_at, win_loss, winloss_indicator) %>% 
              as.data.frame() %>%
              distinct(), by = c("created_at" = "created_at", 
                                      "team_input" = "win_loss")) %>%
  left_join(., winloss_data %>% 
              select(created_at, HomeTeam, total_goals) %>% 
              rename(team_input = HomeTeam) %>% 
              rbind(., winloss_data %>% 
                      select(created_at, AwayTeam, total_goals) %>% 
                      rename(team_input = AwayTeam)) %>% 
              as.data.frame() %>%
              distinct(), by = c("created_at" = "created_at", 
                                 "team_input" = "team_input"))

master_data$winloss_indicator[is.na(master_data$winloss_indicator)] <- 0
master_data <- master_data %>% rename(win_loss = winloss_indicator)

rm(winloss_data)

# ----- Winner Prediction Data -----
training_winner <- master_data %>% 
  filter(created_at <= as.Date(test_date)) %>%
  select(-team_input, -created_at) %>%
  mutate(win_loss_factor = ifelse(win_loss == 0, "L", "W")) %>%
  select(-win_loss, -total_goals) %>%
  rename(win_loss = win_loss_factor) %>%
  as.data.frame()

testing_winner <- master_data %>% 
  filter(created_at > as.Date(test_date)) %>%
  select(-team_input, -created_at) %>%
  mutate(win_loss_factor = ifelse(win_loss == 0, "L", "W")) %>%
  select(-win_loss, -total_goals) %>%
  rename(win_loss = win_loss_factor) %>%
  as.data.frame()

recipe_winner <- recipe(training_winner, formula = win_loss ~ .,)
recipe_winner <- recipe_winner %>%
  step_nzv(all_predictors()) %>%
  step_meanimpute(all_predictors()) %>%
  step_range(min = 0, max = 1, all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 30, role = "predictor") %>%
  prep(retain = TRUE)

prepared_winner <- recipe_winner %>%
  prep(retain = TRUE) %>%
  bake(new_data = training_winner) %>%
  as.data.frame()

test_prepared_winner <- recipe_winner %>%
  prep(retain = TRUE) %>%
  bake(new_data = testing_winner) %>%
  as.data.frame()

model_recipe_winner <- recipe(prepared_winner, formula = win_loss ~ .,)

# ----- Goals Prediction Data -----
training_goals <- master_data %>% 
  filter(created_at <= as.Date(test_date)) %>%
  select(-team_input, -created_at) %>%
  mutate(win_loss_factor = ifelse(total_goals < 2.5, "L", "W")) %>%
  select(-win_loss, -total_goals) %>%
  rename(win_loss = win_loss_factor) %>%
  as.data.frame()


testing_goals <- master_data %>% 
  filter(created_at > as.Date(test_date)) %>%
  select(-team_input, -created_at) %>%
  mutate(win_loss_factor = ifelse(win_loss < 2.5, "L", "W")) %>%
  select(-win_loss, -total_goals) %>%
  rename(win_loss = win_loss_factor) %>%
  as.data.frame()

recipe_goals <- recipe(training_goals, formula = win_loss ~ .,)
recipe_goals <- recipe_goals %>%
  step_nzv(all_predictors()) %>%
  step_meanimpute(all_predictors()) %>%
  step_range(min = 0, max = 1, all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 30, role = "predictor") %>%
  prep(retain = TRUE)

prepared_goals <- recipe_goals %>%
  prep(retain = TRUE) %>%
  bake(new_data = training_goals) %>%
  as.data.frame()

test_prepared_goals <- recipe_goals %>%
  prep(retain = TRUE) %>%
  bake(new_data = testing_goals) %>%
  as.data.frame()

model_recipe_goals <- recipe(prepared_goals, formula = win_loss ~ .,)


# ----- Saving the Output -----
rm(master_data)
save.image("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science/data/production_data/3_modelling_data.RData")