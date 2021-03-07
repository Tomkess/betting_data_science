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
v_last_matches <- 
  betting_ds$run(sql_path = "./db/create views/create - v_last_matches.sql")

v_match_stats <- 
  betting_ds$run(sql_path = "./db/create views/create - v_match_stats.sql")

v_upcoming_matches <- 
  betting_ds$run(sql_path = "./db/create views/create - v_upcoming_matches.sql")
