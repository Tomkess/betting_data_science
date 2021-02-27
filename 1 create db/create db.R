library("generalToolboxR")

# ----- Set Working Directory -----
setwd("c:/Users/Peter/Desktop/ds_projects/betting_data_science/")

# ----- Download t_match_calendar -----
betting_ds <- dbManager$new()
betting_ds$db_config(db_drv = "PostgreSQL",
                     db_name = "betting_ds",
                     host = "localhost",
                     port = 5432,
                     user = "postgres",
                     password = "thatSounds77")

# ------ Create Database -----
betting_ds$run(sql_path = "./db/create db/create-db.sql")