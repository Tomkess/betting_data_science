# ----- Preprocessing a XML feed -----
library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

# ----- Set Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science")

# ----- Load Actual XML Feed -----
load("0_etl/db_temp/1_tipsport_feedxml.RData")

# ----- Load Mapped Data -----
load("2_ml_pipelines/-unique_value_check/mapping_storage/team_mapping.RData")

# ----- Set Current Date ----- #
current_date <- Sys.Date() - 12

xml_data <- 
  data_temp %>%
  filter(sport_league %in% unique(team_mapping$Tipsport_Changed)) %>%
  filter(sport_category %in% "Fotbal - muži" &
           (eventtypedescription %in% c("Vítěz", "Počet gólů (bodů)"))) %>%
  mutate(dateclosed = dmy_hm(dateclosed)) %>%
  as.data.frame() %>%
  
  filter(dateclosed >= current_date) %>%
  as.data.frame()

winner_bets <- 
  xml_data %>% 
  filter(type %in% c("1", "2", "x") & eventname %in% "Výsledek zápasu") %>%
  
  mutate(type_match = ifelse(type %in% "1", "tipsportH", 
                             ifelse(type %in% "x", "tipsportD", 
                                    ifelse(type %in% "2", "tipsportA", fullname)))) %>%
  
  select(-one_of("eventtype", "eventtypedescription", "gamepart", "eventid", 
                 "eventname", "opportunityid", "number", "fullname", "sport_id", 
                 "sport_category", "sport", "livematchid", "annualname", 
                 "url", "urlold", "matchid", "sport_url", "sport_urlold")) %>%
  select(-type) %>%
  
  # - get newest bets
  group_by(dateclosed, namefull, competition_name, sport_league, type_match) %>%
  mutate(max_createdat = max(created_at)) %>%
  as.data.frame() %>%
  filter(created_at == max_createdat) %>%
  select(-max_createdat) %>%
  
  spread(type_match, rate) %>%
  rowwise() %>%
  mutate(HomeTeam = trimws(str_split(competition_name, "-")[[1]][1], which = "both"),
         AwayTeam = trimws(str_split(competition_name, "-")[[1]][2], which = "both")) %>%
  
  as.data.frame() %>%
  left_join(., team_mapping %>% 
              select(Tipsport_Changed, `Tipsport Team [Changed]`, 
                     `Results Team`, `Results League`) %>%
              distinct(), 
            by = c("sport_league" = "Tipsport_Changed", 
                   "HomeTeam" = "Tipsport Team [Changed]")) %>%  
  rename(HomeTeam_tipsport = `Results Team`,
         HomeTeam_League = `Results League`) %>%
  
  left_join(., team_mapping %>% 
              select(Tipsport_Changed, `Tipsport Team [Changed]`, 
                     `Results Team`, `Results League`) %>%
              distinct(), 
            by = c("sport_league" = "Tipsport_Changed", 
                   "AwayTeam" = "Tipsport Team [Changed]")) %>%  
  rename(AwayTeam_tipsport = `Results Team`,
         AwayTeam_League = `Results League`) %>%
  as.data.frame() %>%
  
  select(-competition_name)

goals_number <- xml_data %>%
  filter(type %in% c("u", "o") & gamepart %in% "1/1") %>%
  select(-one_of("eventtype", "eventtypedescription", "gamepart", "eventid", 
                 "eventname", "opportunityid", "number", "sport_id", "type",
                 "sport_category", "sport", "livematchid", "annualname", 
                 "url", "urlold", "matchid", "sport_url", "sport_urlold")) %>%
  distinct() %>%
  
  # - get newest bets
  group_by(fullname, dateclosed, namefull, competition_name, sport_league) %>%
  mutate(max_createdat = max(created_at)) %>%
  as.data.frame() %>%
  filter(created_at == max_createdat) %>%
  select(-max_createdat) %>%
  as.data.frame() %>%
  
  spread(fullname, rate)

nearest_matches <- winner_bets %>%
  left_join(., goals_number)

min_schedule <- 
  nearest_matches %>%
  select(HomeTeam_tipsport, dateclosed) %>%
  distinct() %>%
  group_by(HomeTeam_tipsport) %>%
  mutate(min_dateclosed = min(dateclosed)) %>%
  rename(team = HomeTeam_tipsport) %>%
  
  rbind(., nearest_matches %>%
          select(AwayTeam_tipsport, dateclosed) %>%
          distinct() %>%
          group_by(AwayTeam_tipsport) %>%
          mutate(min_dateclosed = min(dateclosed)) %>%
          rename(team = AwayTeam_tipsport)) %>%
  group_by(team) %>%
  summarise(ult_min_dateclosed = min(min_dateclosed))

nearest_matches <- nearest_matches %>%
  left_join(., min_schedule, by = c("HomeTeam_tipsport" = "team")) %>%
  rename(hometeam_mindateclosed = ult_min_dateclosed) %>%
  
  left_join(., min_schedule, by = c("AwayTeam_tipsport" = "team")) %>%
  rename(awayteam_mindateclosed = ult_min_dateclosed)

nearest_matches <- nearest_matches %>%
  filter(dateclosed == awayteam_mindateclosed & 
           dateclosed == hometeam_mindateclosed)

rm(list = ls()[!(ls() %in% "nearest_matches")])
save.image("3_upcoming_matches/db_temp/nearest_matches.RData")
