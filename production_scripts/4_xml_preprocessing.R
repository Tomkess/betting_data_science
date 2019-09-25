# ----- Pre-processing the XML Feed Data -----

# ---- Library Loading -----
library(dplyr)
library(data.table)

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

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

# ----- Loading the Data -----
data_xml <- readRDS("data/production_data/1_tipsport_feedxml.RData")
master_data <- readRDS(file = "data/production_data/0_data_download.RData")

# ----- Select only specific leagues and two types of bets - win/loss, +/-2.5 ----
data_subset <- data_xml %>%
  filter(sport_category %in% "Fotbal - muži" &
           (eventtypedescription %in% c("Vítěz", "Počet gólů (bodů)")) &
           sport_league %in% football_leagues & gamepart %in% "1/1" & 
           !(type %in% "x") & fullname %in% c("Méně než 2.5", "Více než 2.5")) %>%
  rbind(., data_xml %>%
          filter(sport_category %in% "Fotbal - muži" &
                   eventtypedescription %in% c("Vítěz") &
                   sport_league %in% football_leagues & gamepart %in% "1/1" & 
                   !(type %in% "x")))

# ----- Synchronizing the team Names -----
mapping_table <- master_data %>%
  select(Div, HomeTeam) %>%
  rename(team_input = HomeTeam) %>%
  rbind(., master_data %>%
          select(Div, AwayTeam) %>%
          rename(team_input = AwayTeam)) %>%
  rowwise() %>%
  mutate(team_input = trimws(team_input, which = "both")) %>%
  distinct()

mapping_table <- mapping_table %>%
  left_join(., data_subset %>% 
              select(fullname) %>% 
              distinct() %>% 
              mutate(correct = 1) %>% 
              rename(team_input = fullname))

mapping_table$fullname <- NA
mapping_table$correct[is.na(mapping_table$correct)] <- 0
mapping_table$fullname[mapping_table$correct == 1] <- 
  mapping_table$team_input[mapping_table$correct == 1]

mapping_table$fullname[mapping_table$team_input %in% "Man City"] <- "Manchester C."
mapping_table$fullname[mapping_table$team_input %in% "Man United"] <- "Manchester U."
mapping_table$fullname[mapping_table$team_input %in% "Crystal Palace"] <- "Crystal P."
mapping_table$fullname[mapping_table$team_input %in% "Sheffield United"] <- "Sheffield Utd."
mapping_table$fullname[mapping_table$team_input %in% "Nott'm Forest"] <- "Nottingham"
mapping_table$fullname[mapping_table$team_input %in% "Bristol City"] <- "Bristol C."
mapping_table$fullname[mapping_table$team_input %in% "Cardiff"] <- "Cardiff City"
mapping_table$fullname[mapping_table$team_input %in% "QPR"] <- "Q.P.R."
