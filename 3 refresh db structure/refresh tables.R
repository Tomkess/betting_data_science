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

# ----- Refresh Views -----
t_mapping_league <- 
  betting_ds$run(sql_path = "./db/create db/create-t_mapping_league.sql")

t_mapping_team <- 
  betting_ds$run(sql_path = "./db/create db/create-t_mapping_team.sql")

t_match_calendar <- 
  betting_ds$run(sql_path = "./db/create db/create-t_match_calendar.sql")

t_match_calendar <- 
  betting_ds$run(sql_path = "./db/create db/create-t_match_stats.sql")

t_tipsport_bookmaker <- 
  betting_ds$run(sql_path = "./db/create db/create-t_tipsport_bookmaker.sql")
