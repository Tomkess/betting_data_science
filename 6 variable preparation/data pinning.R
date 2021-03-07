# ----- Initiate Libraries -----
library("generalToolboxR")

# ----- Settin Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science/")

# ----- Refresh Views -----
source("./3 refresh db structure/refresh views.R")

# ----- Connect to Database -----
betting_ds <- dbManager$new()
betting_ds$db_config(db_drv = "PostgreSQL",
                     db_name = "betting_ds",
                     host = "localhost",
                     port = 5432,
                     user = "postgres",
                     password = "thatSounds77")

# ----- Connect to board -----
pins::board_register(board = "local")

# ----- Download Data and Pin it -----
v_last_matches <- 
  betting_ds$run(sql_statement = "select * from v_last_matches;") %>% 
  pin("v_last_matches", board = "local")

v_match_stats <- 
  betting_ds$run(sql_statement = "select * from v_match_stats;") %>% 
  pin("v_match_stats", board = "local")

t_match_stats <- 
  betting_ds$run(sql_statement = "select * from t_match_stats;") %>% 
  pin("t_match_stats", board = "local")

v_upcoming_matches <- 
  betting_ds$run(sql_path = "./db/Scripts/get upcoming matches.sql") %>% 
  pin("v_upcoming_matches", board = "local")

target_vars <- 
  betting_ds$run(sql_path = "./db/Scripts/get target variable.sql") %>% 
  pin("target_vars", board = "local")

t_mapping_team <- 
  betting_ds$run(sql_path = "./db/Scripts/get mapping team.sql") %>% 
  pin("t_mapping_team", board = "local")

# ----- Results Data -----
results_data <-
  pin_get("v_match_stats", "local") %>% 
  as.data.frame() %>% 
  distinct() %>%
  
  dplyr::select(season, league, created_at, team, is_home, 
                match_id, total_goals, n_goals) %>% 
  as.data.frame() %>%
  
  left_join(., pin_get("t_mapping_team", "local") %>% 
              as.data.frame(),
            by = c("team" = "results_team",
                   "league" = "results_league")) %>% 
  as.data.frame() %>% 
  
  dplyr::select(-team, -league) %>% 
  distinct() %>% 
  as.data.frame() %>% 
  
  dplyr::filter(is_home == 1) %>% 
  rename(home_team = tipsport_team,
         h_goals = n_goals) %>%
  dplyr::select(-is_home) %>% 
  
  left_join(., pin_get("v_match_stats", "local") %>% 
              as.data.frame() %>% 
              distinct() %>%
              
              dplyr::select(season, league, created_at, team, is_home, 
                            match_id, total_goals, n_goals) %>% 
              as.data.frame() %>%
              
              left_join(., pin_get("t_mapping_team", "local") %>% 
                          as.data.frame(),
                        by = c("team" = "results_team",
                               "league" = "results_league")) %>% 
              as.data.frame() %>% 
              
              dplyr::select(-team, -league) %>% 
              distinct() %>% 
              as.data.frame() %>% 
              
              dplyr::filter(is_home == 0) %>% 
              rename(away_team = tipsport_team,
                     a_goals = n_goals) %>%
              dplyr::select(-is_home, -total_goals) %>% 
              as.data.frame()) %>% 
  
  na.omit() %>% 
  dplyr::select(-match_id) %>% 
  as.data.frame() %>% 
  pin("results_data", "local")