# ----- XGBOOST model with CV -----
xgb_cv_bayes <- function(eta, max_depth, min_child_weight, 
                         n_folds_input = 30, gamma, alpha, 
                         lambda) {
  
  cv_folds <- KFold(xgboost_traindata$goals, 
                    nfolds = n_folds_input,
                    stratified = FALSE, 
                    seed = 0)
  
  # eta = 0.15
  # max_depth = 2
  # min_child_weight = 3
  # n_folds_input = 30
  # lambda = 1
  # gamma = 0
  # alpha = 1
  
  cv <- xgb.cv(params = list(booster = "gbtree", 
                             eta = eta,
                             max_depth = max_depth,
                             min_child_weight = min_child_weight,
                             subsample = 1, 
                             colsample_bytree = 1,
                             lambda = lambda,
                             alpha = alpha,
                             gamma = gamma,
                             objective = "count:poisson",
                             eval_metric = "poisson-nloglik"),
               
               data = dtrain,
               watchlist = list(test = dtest),
               nrounds = 25,
               print_every_n = 10,
               folds = cv_folds, 
               prediction = F, 
               showsd = F,
               early_stopping_rounds = 5,
               verbose = T)
  
  max_nrounds <- length(cv$evaluation_log$test_poisson_nloglik_mean)
  
  list(Score = (-1) * 
         cv$evaluation_log$test_poisson_nloglik_mean[max_nrounds],
       Pred = cv$pred)
}

# ----- Get Predictions -----
get_predictions <- function(data, model_list){
  
  # data <- master_data %>% filter(Div %in% "E0")
  # model_list <- xgboost_model_output
  # div_input <- "E0"
  
  div_unique <- unique(data$Div)
  
  if(div_unique %in% names(model_list)){
    model_data <- 
      data %>%
      dplyr::select(goals, model_list[[div_unique]]$feature_names) %>%
      as.data.frame()
    
    dtrain <- 
      xgb.DMatrix(data.matrix(model_data %>% 
                                select(-goals) %>% 
                                as.data.frame()), 
                  label = model_data$goals, missing = NaN)
    
    return(c(as.numeric(predict(model_list[[div_unique]], 
                                newdata = dtrain))))
    
  }else{
    
    return(rep(NA, nrow(model_data)))
    
  }
}

# ----- Specify Allocation Budget ----- 
allocation_budget <- function(fr){
  sum(fr)
}

if(which_sharpe == "xml_data"){
  
  # ----- Sharpe Function for Winner (xml_data) -----
  sharpe_winner <- function(fr){
    return((-1)*as.numeric((matrix((xml_data$prob) *
                                     matches_sample$adjusted_odds -
                                     (1 - matches_sample$prob), nrow = 1) %*%
                              t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*%
                                                             diag((matches_sample$prob) *
                                                                    (1-matches_sample$prob) *
                                                                    (matches_sample$adjusted_odds^2)) %*%
                                                             t(t(matrix(fr, ncol = 1)))))))
  }
  
  # ----- Sharpe Function for Goals (xml_data) -----
  sharpe_goals <- function(fr){
    return((-1)*as.numeric((matrix((xml_data$prob_goals) * 
                                     xml_data$goals_odds - 
                                     (1 - xml_data$prob_goals), nrow = 1) %*% 
                              t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*% 
                                                             diag((xml_data$prob_goals) * 
                                                                    (1 - xml_data$prob_goals) * 
                                                                    (xml_data$goals_odds^2)) %*% 
                                                             t(t(matrix(fr, ncol = 1)))))))
  }
  
}

if(which_sharpe == "matches_sample"){
  
  # ----- Sharpe Function for Winner (matches_sample) -----
  sharpe_winner <- function(fr){
    return((-1)*as.numeric((matrix((matches_sample$prob) * 
                                     matches_sample$adjusted_odds - 
                                     (1 - matches_sample$prob), nrow = 1) %*% 
                              t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*% 
                                                             diag((matches_sample$prob) * 
                                                                    (1-matches_sample$prob) * 
                                                                    (matches_sample$adjusted_odds^2)) %*% 
                                                             t(t(matrix(fr, ncol = 1)))))))
  }
  
  # ----- Sharpe Function for Goals (matches_sample) -----
  sharpe_goals <- function(fr){
    return((-1)*as.numeric((matrix((matches_sample$prob_goals) * 
                                     matches_sample$odds_25 - 
                                     (1 - matches_sample$prob_goals), 
                                   nrow = 1) %*% 
                              t(matrix(fr, nrow = 1))/sqrt(t(matrix(fr, ncol = 1)) %*% 
                                                             diag((matches_sample$prob_goals) * 
                                                                    (1-matches_sample$prob_goals) * 
                                                                    (matches_sample$odds_25^2)) %*% 
                                                             t(t(matrix(fr, ncol = 1)))))))
  }
  
}
# ----- Get Probabilities ----- 
get_probabilities <- function(m_pred_input){
  temp <- data.frame("HomeAvg" = m_pred_input$HomeGoals[1], 
                     "AwayAvg" = m_pred_input$AwayGoals[1], 
                     "goals" = c(0:10)) %>%
    rowwise() %>%
    mutate(AwayProb = ((AwayAvg^goals)*(exp(-(AwayAvg))))/(factorial(goals)),
           HomeProb = ((HomeAvg^goals)*(exp(-(HomeAvg))))/(factorial(goals)))
  
  # prob_matrix <- 
  #   matrix(temp$AwayProb) %*% t(matrix(temp$HomeProb))
  prob_matrix <- 
    matrix(0, 
           nrow = nrow(temp), 
           ncol = nrow(temp))
  
  
  for(i_home in 1:length(temp$HomeProb)){
    for(j_away in 1:length(temp$AwayProb)){
      prob_matrix[i_home, j_away] <- 
        temp$HomeProb[i_home] * 
        temp$AwayProb[j_away]
    }
  }
  
  under_25 <- 
    prob_matrix[1,1] + prob_matrix[1,2] + prob_matrix[1,3] +
    prob_matrix[2,1] + prob_matrix[2,2] + 
    prob_matrix[3,1]
  
  out_data <- 
    c("goals_251" = 1 - under_25,
      "goals_25" = under_25,
      "HomeProb" = 
        sum(sum(lower.tri(prob_matrix) * prob_matrix)),
      "AwayProb" = 
        sum(sum(upper.tri(prob_matrix) * prob_matrix)),
      "DrawProb" = 
        1 - 
        sum(sum(upper.tri(prob_matrix) * prob_matrix)) - 
        sum(sum(lower.tri(prob_matrix) * prob_matrix)))
  
  return(out_data)
}

# ----- Get Weights -----
get_weights <- function(B365A, B365D, B365H, 
                        BWA, BWH, BWD,
                        IWA, IWH, IWD,
                        PSA, PSH, PSD,
                        WHA, WHH, WHD,
                        VCA, VCH, VCD){
  
  if(!is.na(B365A) & !is.na(B365H) & !is.na(B365D)){
    
    return((-1) * (100 - 100/B365A - 100/B365H - 100/B365D))
    
  } else if(!is.na(BWA) & !is.na(BWH) & !is.na(BWD)){
    
    return((-1) * (100 - 100/BWA - 100/BWH - 100/BWD))
    
  }else if(!is.na(IWA) & !is.na(IWH) & !is.na(IWD)){
    
    return((-1) * (100 - 100/IWA - 100/IWH - 100/IWD))
    
  }else if(!is.na(PSA) & !is.na(PSH) & !is.na(PSD)){
    
    return((-1) * (100 - 100/PSA - 100/PSH - 100/PSD))
    
  }else if(!is.na(WHA) & !is.na(WHH) & !is.na(WHD)){
    
    return((-1) * (100 - 100/WHA - 100/WHH - 100/WHD))
    
  }else if(!is.na(VCA) & !is.na(VCH) & !is.na(VCD)){
    
    return((-1) * (100 - 100/VCA - 100/VCH - 100/VCD))
    
  }else{
    return(1)
  }
}

# ----- Get Predictions -----
get_predictions <- function(data, model_list){
  
  # data <- master_data %>% filter(Div %in% "E0")
  # model_list <- xgboost_model_output
  # div_input <- "E0"
  
  div_unique <- unique(data$Div)
  
  if(div_unique %in% names(model_list)){
    model_data <- 
      data %>%
      dplyr::select(goals, model_list[[div_unique]]$feature_names) %>%
      as.data.frame()
    
    dtrain <- 
      xgb.DMatrix(data.matrix(model_data %>% 
                                select(-goals) %>% 
                                as.data.frame()), 
                  label = model_data$goals, missing = NaN)
    
    return(c(as.numeric(predict(model_list[[div_unique]], 
                                newdata = dtrain))))
    
  }else{
    
    return(rep(NA, nrow(model_data)))
    
  }
}

# ----- Variable Function -----
get_n_matches_statistics <- function(data_input, team_input, 
                                     match_date, number_matches){
  
  # data_input = master_data
  # team_input = "Liverpool"
  # match_date = "2019-08-09"
  # number_matches = 10
  
  stats_data <- data_input %>%
    filter(created_at < match_date & 
             (HomeTeam %in% team_input | AwayTeam %in% team_input)) %>%
    arrange(desc(created_at)) %>%
    as.data.frame()
  
  if(nrow(stats_data) <= number_matches){
    stats_data <- stats_data
  }else{
    stats_data <- stats_data[1:number_matches,]
  }
  
  # - Get Historical Variables
  historical_stats <- stats_data %>%
    group_by(.) %>%
    summarise(total_goals = 
                sum(c(FTHG[HomeTeam %in% team_input],
                      FTAG[AwayTeam %in% team_input]), na.rm = TRUE),
              total_obtained = 
                sum(c(FTHG[!(HomeTeam %in% team_input)], 
                      FTAG[!(AwayTeam %in% team_input)]), na.rm = TRUE),
              home_matches = 
                length(HomeTeam[HomeTeam %in% team_input])/n(),
              away_matches = 
                length(AwayTeam[AwayTeam %in% team_input])/n(),
              
              # - sum mean sd of number of shots per match
              sum_shots = sum(HS + AS),
              mean_shots = mean(HS + AS),
              sd_shots = sd(HS + AS),
              
              sum_shots_teaminput = 
                sum(c(HS[HomeTeam %in% team_input],
                      AS[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_shots_teaminput = 
                mean(c(HS[HomeTeam %in% team_input], 
                       AS[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_shots_teaminput = 
                sd(c(HS[HomeTeam %in% team_input], 
                     AS[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - mean number of shots on target per match
              sum_shotsontarget = sum(HST + AST),
              mean_shotsontarget = mean(HST + AST),
              sd_shotsontarget = sd(HST + AST),
              
              sum_shotsontarget_teaminput = 
                sum(c(HST[HomeTeam %in% team_input], 
                      AST[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_shotsontarget_teaminput = 
                mean(c(HST[HomeTeam %in% team_input], 
                       AST[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_shotsontarget_teaminput = 
                sd(c(HST[HomeTeam %in% team_input], 
                     AST[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - mean number of fauls per match
              sum_fauls = sum(HF + AF),
              mean_fauls = mean(HF + AF),
              sd_fauls = sd(HF + AF),
              
              sum_fauls_teaminput = 
                sum(c(HF[HomeTeam %in% team_input],
                      AF[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_fauls_teaminput = 
                mean(c(HF[HomeTeam %in% team_input], 
                       AF[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_fauls_teaminput = 
                sd(c(HF[HomeTeam %in% team_input],
                     AF[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - mean number of corners per match
              sum_corners = sum(HC + AC),
              mean_corners = mean(HC + AC),
              sd_corners = sd(HC + AC),
              
              sum_corners_teaminput = 
                sum(c(HC[HomeTeam %in% team_input], 
                      AC[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_corners_teaminput = 
                mean(c(HC[HomeTeam %in% team_input], 
                       AC[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_corners_teaminput = 
                sd(c(HC[HomeTeam %in% team_input], 
                     AC[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - cumulative number of yellow cards in matches
              sum_yellow = sum(HY + AY),
              mean_yellow = mean(HY + AY),
              sd_yellow = sd(HY + AY),
              
              sum_yellow_teaminput = 
                sum(c(HY[HomeTeam %in% team_input],
                      AY[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_yellow_teaminput = 
                mean(c(HY[HomeTeam %in% team_input],
                       AY[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_yellow_teaminput = 
                sd(c(HY[HomeTeam %in% team_input], 
                     AY[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - cumulative number of red cards
              sum_red = sum(HR + AR),
              mean_red = mean(HR + AR),
              sd_red = sd(HR + AR),
              
              sum_red_teaminput = 
                sum(c(HR[HomeTeam %in% team_input],
                      AR[AwayTeam %in% team_input]), na.rm = TRUE),
              mean_red_teaminput = 
                mean(c(HR[HomeTeam %in% team_input],
                       AR[AwayTeam %in% team_input]), na.rm = TRUE),
              sd_red_teaminput = 
                sd(c(HR[HomeTeam %in% team_input],
                     AR[AwayTeam %in% team_input]), na.rm = TRUE),
              
              # - the strength index - the rate of non lost games (win + draws)/matches played
              strength_index = 
                (length(created_at[HomeTeam %in% team_input & FTR %in% "H"]) +
                   length(created_at[AwayTeam %in% team_input & FTR %in% "A"]) +
                   length(created_at[HomeTeam %in% team_input & FTR %in% "D"]) +
                   length(created_at[AwayTeam %in% team_input & FTR %in% "D"]))/n(),
              
              # - shots success rate, i.e. the rate of shots on target, of team_input - mean and sd
              mean_shotsrate_teaminput = 
                mean(c(HST[HomeTeam %in% team_input]/(1 + HS[HomeTeam %in% team_input]),
                       AST[AwayTeam %in% team_input]/(1 + AS[AwayTeam %in% team_input]))),
              
              sd_shotsrate_teaminput = 
                sd(c(HST[HomeTeam %in% team_input]/(1 + HS[HomeTeam %in% team_input]),
                     AST[AwayTeam %in% team_input]/(1 + AS[AwayTeam %in% team_input])))
    ) %>%
    rowwise() %>%
    mutate(rate_scored = total_goals/(total_goals + total_obtained),
           
           # - share variables
           ratio_sum_shots = sum_shots_teaminput/sum_shots,
           ratio_mean_shots = mean_shots_teaminput/mean_shots,
           ratio_sd_shots = sd_shots_teaminput/sd_shots,
           ratio_sum_shotsontarget = sum_shotsontarget_teaminput/sum_shotsontarget,
           ratio_mean_shotsontarget = mean_shotsontarget_teaminput/mean_shotsontarget,
           ratio_sd_shotsontarget = sd_shotsontarget_teaminput/sd_shotsontarget,
           ratio_sum_fauls = sum_fauls_teaminput/sum_fauls,
           ratio_mean_fauls = mean_fauls_teaminput/mean_fauls,
           ratio_sd_fauls = sd_fauls_teaminput/sd_fauls,
           ratio_sum_corners = sum_corners_teaminput/sum_corners,
           ratio_mean_corners = mean_corners_teaminput/mean_corners,
           ratio_sd_corners = sd_corners_teaminput/sd_corners,
           ratio_sum_yellow = sum_yellow_teaminput/sum_yellow,
           ratio_mean_yellow = mean_yellow_teaminput/mean_yellow,
           ratio_sd_yellow = sd_yellow_teaminput/sd_yellow,
           ratio_sum_red = sum_red_teaminput/sum_red,
           ratio_mean_red = mean_red_teaminput/mean_red,
           ratio_sd_red = sd_red_teaminput/sd_red) %>%
    as.data.frame()
  
  # - slope of regression
  beta_slope <- stats_data %>% 
    select(created_at, HomeTeam, AwayTeam, FTR, FTHG, FTAG) %>%
    arrange(created_at) %>%
    mutate(index = row_number(),
           index_sq = row_number() * row_number()) %>%
    mutate(scored_goals = cumsum(replace(FTAG, !(AwayTeam %in% team_input), 0) + replace(FTHG, !(HomeTeam %in% team_input), 0))) %>%
    do(tidy(lm(scored_goals ~ index + index_sq, data = .))) %>%
    filter(term %in% "index") %>%
    select(estimate)
  
  beta_slope <- as.numeric(beta_slope$estimate)
  
  historical_stats$beta_slope <- beta_slope
  # ----- Get Historical Variables ----- #
  
  names(historical_stats) <- paste(names(historical_stats), "_", number_matches, sep = "")
  
  return(historical_stats)
}

