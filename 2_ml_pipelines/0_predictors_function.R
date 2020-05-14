# ----- Library Loading -----
library(purrr)
library(multidplyr)
library(tidyr)
library(rlang)

unnest_dt <- function(dt, col, id){
  
  stopifnot(is.data.table(dt))
  by <- substitute(id)
  col <- substitute(unlist(col, recursive = FALSE))
  dt[, eval(col), by = eval(by)]
  
}

# ----- Get Modelling Data -----
get_modellingdata <- function(.data) {
  
  temp <- .data
  match_data_temp <- match_data %>% 
    filter(team %in% unique(temp$team))
  
  temp_hist_matches <- 
    unnest_dt(as.data.table(temp), 
              hist_match, 
              list(sport, league, match_date, is_home, team, gp_suffix)) %>%
    as.data.frame() %>%
    
    left_join(., match_data_temp %>% rename(is_home_mean = is_home),
              by = c("prev_match_date" = "created_at", 
                     "team" = "team")) %>%
    select(-prev_match_date, -last_n) %>%
    group_by(sport, league, match_date, is_home, team, gp_suffix) %>%
    summarise_all(., mean) %>%
    
    gather(., var_names, var_values,
           -sport, -league, -match_date, -is_home, -team, -gp_suffix) %>%
    as.data.frame() %>%
    mutate(var_names_new = paste(var_names, "_", gp_suffix, sep = "")) %>%
    select(-var_names, -gp_suffix) %>%
    spread(., var_names_new, var_values) %>%
    as.data.frame()
  
  return(temp_hist_matches)
}

# ----- Example -----
# modelling_data <- get_modellingdata(test %>% filter(league %in% "E0"))
