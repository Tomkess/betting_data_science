# ----- Modelling Data Preparation -----
library(data.table)
library(dplyr)
library(recipes)
library(stringr)
library(fastDummies)
library(lubridate)
library(MASS)

rm(list = ls())
gc()

setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")
test_date <- "2018-06-01"

# ----- Get the Predicted Variable -----
load(file = "data/production_data/0_data_download.RData")
winloss_data <- master_data

# ----- Read Variables -----
load(file = "data/production_data/2_variable_calculation.RData")
master_data <- modelling_data
rm(modelling_data)

# ----- Create Modelling Data -----
modelling_data <- 
  
  # - Home Team Results
  winloss_data %>%
  dplyr::select(created_at, Div, HomeTeam, AwayTeam, FTHG) %>%
  group_by(.) %>%
  mutate(home = 1) %>%
  rename(team = HomeTeam,
         opponent = AwayTeam,
         goals = FTHG) %>%
  as.data.frame() %>%
  
  # - Away Team Results
  rbind(., 
        winloss_data %>%
          dplyr::select(created_at, Div, HomeTeam, AwayTeam, FTAG) %>%
          group_by(.) %>%
          mutate(home = 0) %>%
          rename(team = AwayTeam,
                 opponent = HomeTeam,
                 goals = FTAG) %>%
          as.data.frame()) %>%
  group_by(.) %>%
  distinct()

master_data <- master_data %>%
  left_join(., modelling_data, 
            by = c("created_at" = "created_at", "team_input" = "team"))

subset_columns <- 
  names(master_data)[str_detect(names(master_data), "_10") == T]

# ----- Fitting Model -----
model_glm <- glm(goals ~ ., 
                 data = master_data %>%
                   as.data.frame() %>%
                   dplyr::select(-created_at) %>%
                   dplyr::select(c(subset_columns), team_input, 
                                 home, opponent, goals) %>%
                   as.data.frame(), 
                 family = "poisson", )

stepwise_model <- stepAIC(model_glm, 
                          scope = list(lower = ~ team_input + home + opponent, 
                                       upper = model_glm))

library(arm)
model_bayes_glm <- bayesglm(goals ~ ., 
                            data = master_data %>%
                              filter(Div %in% "E0") %>%
                              as.data.frame() %>%
                              dplyr::select(-created_at, -Div) %>%
                              # dplyr::select(c(subset_columns), team_input, 
                              #               home, opponent, goals) %>%
                              na.omit() %>%
                              as.data.frame(), 
                            family = poisson(link = "log"))

stepwise_model <- stepAIC(model_bayes_glm, 
                          scope = list(lower = ~ team_input + home + opponent, 
                                       upper = model_bayes_glm))

# match_a <- data.frame("team" = "Man United",
#                       "opponent" = "Norwich",
#                       "home" = 1)
# 
# match_b <- data.frame("team" = "Liverpool",
#                       "opponent" = "Man United",
#                       "home" = 0)
# 
# temp_output <- data.frame("goals" = c(0:10))
# temp_output$team_a <- NA
# temp_output$team_b <- NA
# 
# lambda_a <- as.numeric(predict.glm(object = model_glm, newdata = match_a))
# lambda_b <- as.numeric(predict.glm(object = model_glm, newdata = match_b))
# 
# for(i in c(0:10)){
#   temp_output$team_a[i+1] <- ((lambda_a^i) * exp(-lambda_a)) / (factorial(i))
#   temp_output$team_b[i+1] <- ((lambda_b^i) * exp(-lambda_b)) / (factorial(i))
# }
# 
# library(ggplot2)
# ggplot(data = temp_output %>% filter(goals <= 5)) +
#   geom_line(aes(x = goals, y = team_a, colour = "Man United")) +
#   geom_line(aes(x = goals, y = team_b, colour = "Liverpool"))
# 
# results_prob <- as.matrix(temp_output$team_a) %*% 
#   t(as.matrix(temp_output$team_b))
# 
# a_wins <- 0
# b_wins <- 0
# draw_results <- 0
# for(i in 1:nrow(results_prob)){
#   for(j in 1:nrow(results_prob)){
#     
#     if(i > j){
#       a_wins <- a_wins + results_prob[i,j]
#     }
#     
#     if(i < j){
#       b_wins <- b_wins + results_prob[i,j]
#     }
#     
#     if(i == j){
#       draw_results <- draw_results + results_prob[i,j]
#     }
#     
#   }
# }
