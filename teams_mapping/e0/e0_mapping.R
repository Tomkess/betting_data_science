# ----- Preprocessing a XML feed -----
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

rm(list = ls())
gc()
which_sharpe <- "xml_data"

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
source("functions.R")

# ----- Load Actual XML Feed ----- #
load("data/production_data/1_tipsport_feedxml.RData")

# ----- Watched Leagues -----
football_leagues <- c("1. anglická liga")

# ----- Set Current Date ----- #
current_date <- Sys.Date()

# ----- Loading the Data -----
load("data/production_data/0_data_download.RData")

# ----- Select only specific leagues and two types of bets - win/loss, +/-2.5 ----
data_subset <- data_temp %>%
  filter(sport_category %in% "Fotbal - muži" &
           (eventtypedescription %in% c("Vítěz", "Počet gólů (bodů)")) &
           sport_league %in% football_leagues & 
           !(type %in% c("x", "o", "u"))) %>%
  rbind(., data_temp %>%
          filter(sport_category %in% "Fotbal - muži" &
                   eventtypedescription %in% c("Vítěz", "Počet gólů (bodů)") &
                   sport_league %in% football_leagues & 
                   !(type %in% c("x", "o", "u")))) %>%
  as.data.frame() %>%
  select(fullname) %>% 
  distinct() %>% 
  mutate(correct = 1) %>% 
  rename(team_input = fullname)

# ----- Synchronizing the team Names -----
mapping_table <- master_data %>%
  select(Div, HomeTeam) %>%
  rename(team_input = HomeTeam) %>%
  rbind(., master_data %>%
          select(Div, AwayTeam) %>%
          rename(team_input = AwayTeam)) %>%
  rowwise() %>%
  mutate(team_input = trimws(team_input, which = "both")) %>%
  distinct() %>% 
  left_join(., data_subset) %>%
  select(-Div) %>%
  distinct() %>%
  as.data.frame()

mapping_table$fullname <- NA
mapping_table$correct[is.na(mapping_table$correct)] <- 0
mapping_table$fullname[mapping_table$correct == 1] <- 
  mapping_table$team_input[mapping_table$correct == 1]

# - English League: E
mapping_table$fullname[mapping_table$team_input %in% "Man City"] <- "Manchester City"
mapping_table$fullname[mapping_table$team_input %in% "Man United"] <- "Manchester United"
mapping_table$fullname[mapping_table$team_input %in% "Crystal Palace"] <- "Crystal Palace"
mapping_table$fullname[mapping_table$team_input %in% "Sheffield United"] <- "Sheffield Utd."
mapping_table$fullname[mapping_table$team_input %in% "Nott'm Forest"] <- "Nottingham"
mapping_table$fullname[mapping_table$team_input %in% "Bristol City"] <- "Bristol C."
mapping_table$fullname[mapping_table$team_input %in% "Cardiff"] <- "Cardiff City"
mapping_table$fullname[mapping_table$team_input %in% "QPR"] <- "Q.P.R."
mapping_table$fullname[mapping_table$team_input %in% "Sheffield Weds"] <- "Sheffield W."
mapping_table$fullname[mapping_table$team_input %in% "West Brom"] <- "W. B. Albion"
mapping_table$fullname[mapping_table$team_input %in% "Middlesboro"] <- "Middlesbrough"
mapping_table$fullname[mapping_table$team_input %in% "Wimbledon"] <- "AFC Wimbledon"
mapping_table$fullname[mapping_table$team_input %in% "AFC Telford United"] <- "Telford"
mapping_table$fullname[mapping_table$team_input %in% "Alfreton Town"] <- "Alfreton"
mapping_table$fullname[mapping_table$team_input %in% "Altrincham"] <- "Altrincham"
mapping_table$fullname[mapping_table$team_input %in% "Bath City"] <- "Bath City"
mapping_table$fullname[mapping_table$team_input %in% "Boston"] <- "Boston Utd."
mapping_table$fullname[mapping_table$team_input %in% "Bristol Rvs"] <- "Bristol Rovers"
mapping_table$fullname[mapping_table$team_input %in% "Braintree Town"] <- "Braintree"
mapping_table$fullname[mapping_table$team_input %in% "Boreham Wood"] <- "Boreham"
mapping_table$fullname[mapping_table$team_input %in% "Burton"] <- "Burton"
mapping_table$fullname[mapping_table$team_input %in% "York"] <- "York"
mapping_table$fullname[mapping_table$team_input %in% "Weymouth"] <- "Weymouth"
mapping_table$fullname[mapping_table$team_input %in% "Crawley Town"] <- "Crawley"
mapping_table$fullname[mapping_table$team_input %in% "Darlington"] <- "Darlington"
mapping_table$fullname[mapping_table$team_input %in% "Dartford"] <- "Dartford"
mapping_table$fullname[mapping_table$team_input %in% "Fleetwood Town"] <- "Fleetwood"
mapping_table$fullname[mapping_table$team_input %in% "Leyton Orient"] <- "Leyton"
mapping_table$fullname[mapping_table$team_input %in% "Lincoln"] <- "Lincoln"
mapping_table$fullname[mapping_table$team_input %in% "Dover Athletic"] <- "Dover Athletic"
mapping_table$fullname[mapping_table$team_input %in% "Eastbourne Borough"] <- "Eastbourne Borough"
mapping_table$fullname[mapping_table$team_input %in% "Hereford"] <- "Hereford"
mapping_table$fullname[mapping_table$team_input %in% "Peterboro"] <- "Peterborough"
mapping_table$fullname[mapping_table$team_input %in% "Newport County"] <- "Newport"
mapping_table$fullname[mapping_table$team_input %in% "Dag and Red"] <- "Dagenham"
mapping_table$fullname[mapping_table$team_input %in% "Notts County"] <- "Notts"
mapping_table$fullname[mapping_table$team_input %in% "Maidstone"] <- "Maidstone United"
mapping_table$fullname[mapping_table$team_input %in% "Gateshead"] <- "Gateshead"
mapping_table$fullname[mapping_table$team_input %in% "Havant & Waterlooville"] <- "Havant"
mapping_table$fullname[mapping_table$team_input %in% "Guiseley"] <- "Guiseley"
mapping_table$fullname[mapping_table$team_input %in% "Chester"] <- "Chester"
mapping_table$fullname[mapping_table$team_input %in% "Southport"]	<- "Southport"
mapping_table$fullname[mapping_table$team_input %in% "Kidderminster"]	<- "Kiderminster"
mapping_table$fullname[mapping_table$team_input %in% "Welling United"] <- "Welling"
mapping_table$fullname[mapping_table$team_input %in% "Telford United"] <- "Telford"
mapping_table$fullname[mapping_table$team_input %in% "Kettering Town"] <- "Kettering"
mapping_table$fullname[mapping_table$team_input %in% "Farsley"]	<- "Farsley"
mapping_table$fullname[mapping_table$team_input %in% "St. Albans"] <- "St. Albans City"
mapping_table$fullname[mapping_table$team_input %in% "Kidderminster"] <- "Kidderminster"

save(mapping_table, file = "data/production_data/4_xml_preprocessing.RData")


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
