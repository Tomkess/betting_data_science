# ----- Library Loading -----
library(data.table)
library(dplyr)
library(recipes)

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

load("data/production_data/3_modelling_data.RData")
rm(prepared_winner)
rm(prepared_goals)

load("data/production_data/5_model_estimation.RData")
xml_mapping <- readRDS("data/production_data/4_xml_preprocessing.RData")
master_data <- readRDS("data/production_data/2_variable_calculation.RData")
data_xml <- readRDS("data/production_data/1_tipsport_feedxml.RData")

# ----- Watched Leagues -----
football_leagues <-
  c("1. anglická liga",
    "2. anglická liga",
    "3. anglická liga",
    "4. anglická liga",
    "5. anglická liga",
    
    "1. skotská liga",
    "2. skotská liga",
    "3. skotská liga",
    "4. skotská liga",
    
    "1. německá liga",
    "2. německá liga",
    
    "1. italská liga",
    "2. italská liga",
    
    "1. španělská liga",
    "2. španělská liga",
    
    "1. francouzská liga",
    "2. francouzská liga",
    
    "1. nizozemská liga",
    
    "1. dánská liga",
    
    "1. portugalská liga",
    
    "1. turecká liga",
    
    "1. řecká liga")

# ----- Select only specific leagues and two types of bets - win/loss, +/-2.5 ----
xml_subset <- data_xml %>%
  filter(sport_category %in% "Fotbal - muži" &
           (eventtypedescription %in% c("Vítěz", "Počet gólů (bodů)")) &
           sport_league %in% football_leagues & gamepart %in% "1/1" & 
           !(type %in% "x") & 
           fullname %in% c("Méně než 2.5", "Více než 2.5")) %>%
  rbind(., data_xml %>%
          filter(sport_category %in% "Fotbal - muži" &
                   eventtypedescription %in% c("Vítěz") &
                   sport_league %in% football_leagues & 
                   gamepart %in% "1/1" & 
                   !(type %in% "x"))) %>%
  as.data.frame() %>%
  select(-eventtype, -eventtypedescription, -gamepart, -eventid, 
         -eventname, -opportunityid, -number, -matchid, -livematchid,
         -sport_id, -sport_category, -sport, -annualname) %>%
  rowwise() %>%
  mutate(dateclosed = as.Date(lubridate::dmy_hm(dateclosed))) %>%
  rename(matchday = dateclosed)

unique_matches <- xml_subset %>%
  select(fullname, dateclosed, created_at) %>%
  distinct()

# ----- Apply Models -----
for(i in 1:nrow(unique_matches)){
  # i <- 1598
  if(unique_matches$fullname[i] %in% xml_mapping$fullname){
    
    # - subset covariates right before game
    subset_team <- master_data %>%
      filter(team_input %in% xml_mapping$team_input[xml_mapping$fullname %in% unique_matches$fullname[i]] & 
               created_at < as.Date(unique_matches$dateclosed[i])) %>%
      arrange(desc(created_at)) %>%
      as.data.frame() %>%
      select(-team_input, -created_at)

    # - apply recipe_winner
    team_data <- bake(recipe_winner, new_data = subset_team)[1,] %>% 
      as.data.frame() %>%
      select(-win_loss)
    
    # - apply model
    pred_team <- predict_helper(performance_comparison[[1]]$model_fit_object, 
                                team_data, 
                                model_recipe_winner) %>%
      select(-obs)
  }
  
}