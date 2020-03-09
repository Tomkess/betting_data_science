# ----- Preprocessing a XML feed -----
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Load Actual XML Feed ----- #
load("data/production_data/1_tipsport_feedxml.RData")
load("data/production_data/4_xml_preprocessing.RData")

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

# ----- Set Current Date ----- #
current_date <- Sys.Date()

xml_data <- 
  data_temp %>%
  filter(sport_league %in% football_leagues & 
           sport_category %in% "Fotbal - muži" &
           eventname %in% c("Výsledek zápasu", "Počet gólů v zápasu") &
           str_detect(fullname, pattern = "\\(") == F) %>%
  mutate(dateclosed = dmy_hm(dateclosed)) %>%
  as.data.frame() %>%
  
  filter(dateclosed >= current_date) %>%
  as.data.frame() %>%
  
  select(-eventid, -eventtype, -opportunityid, -number, -gamepart, 
         -eventtypedescription, -matchid, -livematchid, -url, -urlold,
         -sport_id, -sport_category, -sport, -annualname, 
         -competition_name, -eventname) %>%
  as.data.frame() %>%
  
  rowwise() %>%
  mutate(HomeTeam = trimws(str_split(namefull, "-")[[1]][1], which = "both"),
         AwayTeam = trimws(str_split(namefull, "-")[[1]][2], which = "both")) %>%
  as.data.frame() %>%
  
  rowwise() %>%
  mutate(type_match = ifelse(type %in% "1", "tipsportH", 
                             ifelse(type %in% "x", "tipsportD", 
                                    ifelse(type %in% "2", "tisportA", fullname)))) %>%
  rename(match_name = namefull) %>%
  select(-type, -fullname) %>%
  as.data.frame() %>%
  spread(type_match, rate) %>%
  
  as.data.frame() %>%
  left_join(., mapping_table %>% 
              select(-correct) %>% 
              distinct(), by = c("HomeTeam" = "fullname")) %>%
  rename(HomeTeam_tipsport = team_input) %>%
  left_join(., mapping_table %>% 
              select(-correct) %>% 
              distinct(), by = c("AwayTeam" = "fullname")) %>%
  rename(AwayTeam_tipsport = team_input) %>%
  as.data.frame() %>%
  
  # - calculate mean for English Premier League
  group_by(dateclosed, match_name, sport_league, sport_url, sport_urlold, 
           HomeTeam, AwayTeam, HomeTeam_tipsport, AwayTeam_tipsport) %>%
  summarise_all(., mean, na.rm = TRUE) %>%
  filter(sport_league %in% "1. anglická liga") %>%
  as.data.frame()

nearest_matches <-
  xml_data %>%
  select(HomeTeam, dateclosed) %>%
  group_by(HomeTeam) %>%
  mutate(min_date = min(dateclosed)) %>%
  rename(team_input = HomeTeam) %>%
  
  rbind(., xml_data %>%
          select(AwayTeam, dateclosed) %>%
          group_by(AwayTeam) %>%
          mutate(min_date = min(dateclosed)) %>%
          rename(team_input = AwayTeam)) %>%
  group_by(team_input) %>%
  summarise(min_date = min(min_date))

xml_data <- xml_data %>%
  left_join(., nearest_matches, by = c("HomeTeam" = "team_input")) %>%
  rename(HomeTeam_mindate = min_date) %>%
  left_join(., nearest_matches, by = c("AwayTeam" = "team_input")) %>%
  rename(AwayTeam_mindate = min_date)

xml_data <- xml_data %>%
  filter(HomeTeam_mindate == AwayTeam_mindate) %>%
  select(-HomeTeam_mindate, -AwayTeam_mindate)
