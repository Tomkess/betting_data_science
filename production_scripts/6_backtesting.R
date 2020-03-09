# ----- Library Loading -----
library(data.table)
library(dplyr)
library(recipes)
library(yardstick)
library(ggplot2)
library(doParallel)
library(foreach)
library(Rsolnp)

test_date <- "2018-06-01"

predict_helper <- function(fit_input, new_data, recipe_input){
  
  if (inherits(new_data, "rsplit")) {
    obs <- as.integer(new_data, data = "assessment")
    
    new_data <- bake(recipe_input, assessment(new_data))
  } else {
    obs <- 1:nrow(new_data)
    new_data <- bake(recipe_input, new_data)
  }
  hold_on <- new_data$win_loss
  new_data$win_loss <- NULL
  
  pred_model <- predict(fit_input, new_data, type = "prob") %>%
    mutate(obs = obs)
  return(pred_model)
}

# ----- Loading Underlying Data -----
load("data/production_data/5_model_estimation.RData")
match_data <- readRDS("data/production_data/0_data_download.RData")
master_data <- readRDS("data/production_data/2_variable_calculation.RData")

# ----- Subset Models -----
models_subset <- bind_rows(lapply(c(1:length(performance_comparison)),
                                  function(x) performance_comparison[[x]][["training_performance"]] %>%
                                    group_by(model_id, grid_id) %>%
                                    summarise(.estimate = mean(.estimate, na.rm = TRUE)))) %>%
  group_by(grid_id) %>%
  filter(.estimate >= 0.605) %>%
  select(grid_id) %>%
  distinct() %>%
  as.data.frame()

# ----- Get Train Matches Prediction -----
train_data <- master_data %>%
  filter(created_at <= as.Date(test_date)) %>%
  filter(team_input %in% unique_teams)

prediction_train <- recipe_winner %>%
  prep(retain = TRUE) %>%
  bake(new_data = train_data) %>%
  as.data.frame()

predicted_train <- bind_rows(lapply(c(models_subset$grid_id), 
                                    function(X) 
                                      predict_helper(fit_input = performance_comparison[[X]]$model_fit_object, 
                                                     new_data = prediction_train, 
                                                     recipe_input = model_recipe_goals))) %>%
  as.data.frame() %>%
  group_by(obs) %>%
  summarise(.pred_L = mean(.pred_L, na.rm = TRUE),
            .pred_W = mean(.pred_W, na.rm = TRUE)) %>%
  select(-obs)

predicted_train$created_at <- train_data$created_at
predicted_train$team_input <- train_data$team_input

match_data <- match_data %>%
  left_join(., predicted_train, by = c("HomeTeam" = "team_input", 
                                       "created_at" = "created_at")) %>%
  rename(homeW_train = .pred_W,
         homeL_train = .pred_L) %>%
  left_join(., predicted_train, by = c("AwayTeam" = "team_input", 
                                       "created_at" = "created_at")) %>%
  rename(awayW_train = .pred_W,
         awayL_train = .pred_L) %>%
  rowwise() %>%
  mutate(home_train = homeW_train/2 + awayL_train/2,
         away_train = homeL_train/2 + awayW_train/2)

match_data$is_bet <- 1
# match_data$is_bet[!(is.na(match_data$home_train)) & 
#                     match_data$homeL_train > match_data$homeW_train & 
#                     match_data$awayL_train > match_data$awayW_train] <- 0
# match_data$is_bet[!(is.na(match_data$home_train)) & 
#                     match_data$homeW_train > match_data$homeL_train & 
#                     match_data$awayW_train > match_data$awayL_train] <- 0

rm(train_data)
rm(predicted_train)
rm(prediction_train)

# ----- Get Test Matches Prediction -----
predicted_test <- bind_rows(lapply(c(models_subset$grid_id), 
                                   function(X) 
                                     predict_helper(fit_input = performance_comparison[[X]]$model_fit_object, 
                                                    new_data = recipe_winner %>%
                                                      prep(retain = TRUE) %>%
                                                      bake(new_data = master_data %>%
                                                             filter(created_at > as.Date(test_date) & created_at <= as.Date("2019-06-01")) %>%
                                                             filter(team_input %in% unique_teams) %>%
                                                             as.data.frame()) %>%
                                                      as.data.frame(), 
                                                    recipe_input = model_recipe_goals))) %>%
  as.data.frame() %>%
  group_by(obs) %>%
  summarise(.pred_L = mean(.pred_L, na.rm = TRUE),
            .pred_W = mean(.pred_W, na.rm = TRUE)) %>%
  select(-obs) %>%
  cbind(., master_data %>%
          filter(created_at > as.Date(test_date) & created_at <= as.Date("2019-06-01")) %>%
          filter(team_input %in% unique_teams) %>%
          as.data.frame() %>%
          select(team_input, created_at))

match_data <- match_data %>%
  left_join(., predicted_test, by = c("HomeTeam" = "team_input", 
                                      "created_at" = "created_at")) %>%
  rename(homeW_test = .pred_W,
         homeL_test = .pred_L) %>%
  left_join(., predicted_test, by = c("AwayTeam" = "team_input", 
                                      "created_at" = "created_at")) %>%
  rename(awayW_test = .pred_W,
         awayL_test = .pred_L) %>%
  rowwise() %>%
  mutate(home_test = homeW_test/2 + awayL_test/2,
         away_test = homeL_test/2 + awayW_test/2)

# match_data$is_bet[!(is.na(match_data$home_test)) & 
#                     match_data$homeL_test > match_data$homeW_test & 
#                     match_data$awayL_test > match_data$awayW_test] <- 0
# match_data$is_bet[!(is.na(match_data$home_test)) & 
#                     match_data$homeW_test > match_data$homeL_test & 
#                     match_data$awayW_test > match_data$awayL_test] <- 0

# ----- Evaluate Betting Strategy on Training Sample-----
dates_train <- seq.Date(from = as.Date(test_date) - 360, 
                        to = as.Date(test_date), by = 1)
initial_investment <- 100000

betting_train <- data.frame("created_at" = dates_train,
                            "number_matches" = rep(0, length(dates_train)),
                            "bet" = rep(0, length(dates_train)),
                            "bet_result" = rep(0, length(dates_train)),
                            "state_investment" = rep(0, length(dates_train)))

betting_train$number_matches[1] <- 0
betting_train$bet[1] <- 0
betting_train$bet_result[1] <- 0
betting_train$state_investment[1] <- initial_investment

for(i in 2:length(dates_train)){
  
  # i <- 2
  match_subset <- match_data %>%
    filter(created_at %in% dates_train[i] & is_bet > 0)
  
  if(nrow(match_subset) > 1){
    
    match_subset <- match_subset %>%
      rowwise() %>%
      mutate(prob = max(home_train, away_train),
             bet_team = ifelse(home_train > away_train, "H", "A"),
             bet_expwin = ifelse(home_train > away_train, 
                                 coalesce(B365H, BWH, IWH, PSH, WHH, VCH, LBH, SJH, GBH, BSH, SBH, SOH, SYH), 
                                 coalesce(B365A, BWA, IWA, PSA, WHA, VCA, LBA, SJA, GBA, BSA, SBA, SOA, SYA))) %>%
      select(HomeTeam, AwayTeam, prob, bet_team, FTR, bet_expwin) %>%
      na.omit()
    
    if(nrow(match_subset) > 4){
      
      # - Optimization
      allocation_budget <- function(fr){
        sum(fr)
      }
      
      sharpe_opt <- function(fr){
        return((-1)*as.numeric((matrix((match_subset$prob) * match_subset$bet_expwin - (1 - match_subset$prob), nrow = 1) %*% t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*% diag((match_subset$prob) * (1-match_subset$prob) * (match_subset$bet_expwin^2)) %*% t(t(matrix(fr, ncol = 1)))))))
      }
      
      opt_bet <- solnp(pars = c(rep(0.01, nrow(match_subset))), 
                       fun = sharpe_opt, eqfun = allocation_budget, 
                       eqB = 0.2,
                       LB = c(rep(0.0, nrow(match_subset))),
                       UB = c(rep(1, nrow(match_subset))))
      
      match_subset$share_opt <- opt_bet$pars
      match_subset$investment <- match_subset$share_opt * 
        betting_train$state_investment[i - 1]
      
      match_subset$result <- (-1) * match_subset$investment
      match_subset$result[match_subset$bet_team == match_subset$FTR] <- 
        match_subset$investment[match_subset$bet_team == match_subset$FTR] * 
        (match_subset$bet_expwin[match_subset$bet_team == match_subset$FTR] - 1)
      
      # - Optimization
      
      betting_train$number_matches[i] <- nrow(match_subset)
      betting_train$bet[i] <- sum(match_subset$investment)
      betting_train$bet_result[i] <- sum(match_subset$result)
      betting_train$state_investment[i] <- 
        sum(match_subset$result) + 
        betting_train$state_investment[i - 1]}else{
          betting_train$number_matches[i] <- nrow(match_subset)
          betting_train$bet[i] <- 0
          betting_train$bet_result[i] <- 0
          betting_train$state_investment[i] <- betting_train$state_investment[i - 1]
        }
    
  }else{
    betting_train$number_matches[i] <- nrow(match_subset)
    betting_train$bet[i] <- 0
    betting_train$bet_result[i] <- 0
    betting_train$state_investment[i] <- betting_train$state_investment[i - 1]
  }
  
  print(i)
}

ggplot(data = betting_train) +
  geom_line(aes(x = created_at, y = state_investment))

# ----- Evaluate Betting Strategy on Testing Sample -----
dates_test <- seq.Date(from = as.Date(test_date), 
                       to = as.Date("2019-06-01"), by = 1)
initial_investment <- 100000

betting_test <- data.frame("created_at" = dates_test,
                           "number_matches" = rep(0, length(dates_test)),
                           "bet" = rep(0, length(dates_test)),
                           "bet_result" = rep(0, length(dates_test)),
                           "state_investment" = rep(0, length(dates_test)))

betting_test$number_matches[1] <- 0
betting_test$bet[1] <- 0
betting_test$bet_result[1] <- 0
betting_test$state_investment[1] <- initial_investment

for(i in 2:length(dates_test)){
  
  # i <- 314
  match_subset <- match_data %>%
    filter(created_at %in% dates_test[i] & is_bet > 0)
  
  if(nrow(match_subset) > 3){
    
    match_subset <- match_subset %>%
      rowwise() %>%
      mutate(prob = max(home_test, away_test),
             bet_team = ifelse(home_test > away_test, "H", "A"),
             bet_expwin = ifelse(home_test > away_test, 
                                 coalesce(B365H, BWH, IWH, PSH, WHH, VCH, LBH, SJH, GBH, BSH, SBH, SOH, SYH), 
                                 coalesce(B365A, BWA, IWA, PSA, WHA, VCA, LBA, SJA, GBA, BSA, SBA, SOA, SYA))) %>%
      select(HomeTeam, AwayTeam, prob, bet_team, FTR, bet_expwin, awayL_test, homeL_test, awayW_test, homeW_test) %>%
      na.omit()
    
    if(nrow(match_subset) > 3){
      
      # - Optimization
      allocation_budget <- function(fr){
        sum(fr)
      }
      
      sharpe_opt <- function(fr){
        return((-1)*as.numeric((matrix((match_subset$prob) * match_subset$bet_expwin - (1 - match_subset$prob), nrow = 1) %*% t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*% diag((match_subset$prob) * (1-match_subset$prob) * (match_subset$bet_expwin^2)) %*% t(t(matrix(fr, ncol = 1)))))))
      }
      
      opt_bet <- solnp(pars = c(rep(0.01, nrow(match_subset))), 
                       fun = sharpe_opt, 
                       eqfun = allocation_budget, 
                       eqB = 0.2,
                       LB = c(rep(0.0, nrow(match_subset))),
                       UB = c(rep(1, nrow(match_subset))))
      
      match_subset$share_opt <- opt_bet$pars
      match_subset <- match_subset %>%
        mutate(share_opt = ifelse(homeL_test > homeW_test & 
                                    awayL_test > awayW_test, 0, share_opt)) %>%
        arrange(desc(prob)) %>%
        .[1:21,] %>%
        na.omit()
      
      match_subset$investment <- match_subset$share_opt * 
        betting_test$state_investment[i - 1]
      
      match_subset$result <- (-1) * match_subset$investment
      match_subset$result[match_subset$bet_team == match_subset$FTR] <- 
        match_subset$investment[match_subset$bet_team == match_subset$FTR] * 
        (match_subset$bet_expwin[match_subset$bet_team == match_subset$FTR] - 1)
      
      # - Optimization
      
      betting_test$number_matches[i] <- nrow(match_subset)
      betting_test$bet[i] <- sum(match_subset$investment)
      betting_test$bet_result[i] <- sum(match_subset$result)
      betting_test$state_investment[i] <- 
        sum(match_subset$result) + 
        betting_test$state_investment[i - 1]
    }else{
      betting_test$number_matches[i] <- nrow(match_subset)
      betting_test$bet[i] <- 0
      betting_test$bet_result[i] <- 0
      betting_test$state_investment[i] <- betting_test$state_investment[i - 1]
    }
    
  }else{
    betting_test$number_matches[i] <- nrow(match_subset)
    betting_test$bet[i] <- 0
    betting_test$bet_result[i] <- 0
    betting_test$state_investment[i] <- betting_test$state_investment[i - 1]
  }
  
  print(i)
}

ggplot(data = betting_test) +
  geom_line(aes(x = created_at, y = state_investment))
