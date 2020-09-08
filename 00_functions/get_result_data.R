# ----- Determine the Round during the season -----
get_result_data <- function(dfit_input, i_c_basic, i_c_input, i_round_data){
  
  result_data <- 
    dfit_input %>%
    
    dplyr::select(one_of(i_c_input)) %>%
    gather("model", "pred", -all_of(i_c_basic)) %>%
    as.data.frame() %>%
    distinct() %>%
    
    rowwise() %>%
    mutate(season = list(i_round_data %>% 
                           filter(match_date >= from_date & 
                                    match_date <= to_date))) %>%
    
    unnest(c(season)) %>%
    as.data.frame() %>%
    dplyr::select(-from_date, -to_date) %>%
    
    group_by(league, season, team) %>%
    arrange(match_date) %>%
    mutate(round = row_number())
  
  return(result_data)
}