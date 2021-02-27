# ----- Initiate Libraries -----
library("generalToolboxR")

# ----- Settin Working Directory -----
setwd("C:/Users/Peter/Desktop/ds_projects/betting_data_science/")

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