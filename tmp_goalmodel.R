library(goalmodel)

TRAIN_DT <- "2018-01-01"
TEST_DT <- "2020-01-01"

pins::board_register(board = "local")

tmp_data <- 
  
  master_data$data[[1]] %>% 
  dplyr::filter(league %in% "E0" & season %in% c("1718", "1819", "1920") & is_home == 0) %>%
  rename(awayteam = team,
         at_goals = n_goals) %>% 
  dplyr::select(created_at, awayteam, match_id, at_goals) %>% 
  as.data.frame() %>% 
  
  left_join(.,   master_data$data[[1]] %>% 
              dplyr::filter(league %in% "E0" & season %in% c("1718", "1819", "1920") & is_home == 1) %>%
              rename(hometeam = team,
                     ht_goals = n_goals) %>% 
              dplyr::select(created_at, hometeam, match_id, ht_goals) %>% 
              as.data.frame()) %>% 
  na.omit() %>% 
  as.data.frame()

tmp_data$weights_dc <- weights_dc(tmp_data$created_at, xi = 0.0019)
gm_res_rs <- goalmodel(goals1 = tmp_data$ht_goals, goals2 = tmp_data$at_goals,
                       team1 = tmp_data$hometeam, team2 = tmp_data$awayteam,
                       rs = TRUE, weights = tmp_data$weights_dc)

to_predict1 <- c('Arsenal', 'Man United', 'Watford')
to_predict2 <- c('Everton', 'Chelsea', 'Liverpool')

predict_expg(gm_res_rs, 
             team1 = to_predict1, 
             team2 = to_predict2, return_df = TRUE)

summary(gm_res_rs)
