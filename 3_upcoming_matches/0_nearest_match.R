# ----- Preprocessing a XML feed -----
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Load Actual XML Feed -----
load("0_etl/db_temp/1_tipsport_feedxml.RData")

# ----- Load Mapped Data -----
load("2_ml_pipelines/-unique_value_check/mapping_storage/team_mapping.RData")

# ----- Set Current Date ----- #
current_date <- Sys.Date() - 120

xml_data <- 
  data_temp %>%
  filter(sport_league %in% unique(team_mapping$Tipsport_Changed) & 
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
  left_join(., team_mapping %>% 
              select(Tipsport_Changed, 
                     `Tipsport Team [Changed]`, `Results Team`) %>%
              distinct(), 
            by = c("sport_league" = "Tipsport_Changed", 
                   "HomeTeam" = "Tipsport Team [Changed]")) %>%  
  rename(HomeTeam_tipsport = `Results Team`) %>%
  
  left_join(., team_mapping %>% 
              select(Tipsport_Changed, `Tipsport Team [Changed]`, 
                     `Results Team`) %>%
              distinct(), 
            by = c("sport_league" = "Tipsport_Changed", 
                   "HomeTeam" = "Tipsport Team [Changed]")) %>%  
  rename(AwayTeam_tipsport = `Results Team`) %>%
  as.data.frame()

# - Duplicates might appear!!!! (same team changes the name over time!!!)

# - calculate mean for English Premier League
# group_by(dateclosed, match_name, sport_league, sport_url, sport_urlold, 
#          HomeTeam, AwayTeam, HomeTeam_tipsport, AwayTeam_tipsport) %>%
# summarise_all(., mean, na.rm = TRUE) %>%
# filter(sport_league %in% "1. anglická liga") %>%
# as.data.frame()

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
  # - get nearest match of HomeTeam
  left_join(., nearest_matches, by = c("HomeTeam" = "team_input")) %>%
  rename(HomeTeam_mindate = min_date) %>%
  
  # - get nearest match of AwayTeam
  left_join(., nearest_matches, by = c("AwayTeam" = "team_input")) %>%
  rename(AwayTeam_mindate = min_date)

xml_data <- xml_data %>%
  filter(HomeTeam_mindate == AwayTeam_mindate) %>%
  select(-HomeTeam_mindate, -AwayTeam_mindate)
