library(purrr)
library(multidplyr)
library(tidyr)

# ----- Get Modelling Data -----
get_modellingdata <- function(.data) {
  
  cluster <- new_cluster(5)
  cluster_copy(cluster, "match_data")
  cluster_library(cluster, "purrr")
  cluster_library(cluster, "dplyr")
  cluster_library(cluster, "tidyr")
  
  test_league <- 
    .data %>%
    as.data.frame() %>%
    group_by(sport, league, match_date, is_home, team, gp_suffix) %>%
    partition(., cluster = cluster) %>%
    mutate(vars_data = 
             map2(hist_match, team,
                  function(data_i, team_i){
                    
                    return(data_i %>%
                             left_join(., match_data %>%
                                         filter(team %in% team_i),
                                       by = c("prev_match_date" = 
                                                "created_at")) %>%
                             select(-last_n, -prev_match_date, -team) %>%
                             rename(is_home_mean = is_home) %>%
                             summarise_all(., mean))
                  })) %>%
    collect() %>%
    unnest(c(vars_data)) %>%
    
    select(-hist_match) %>%
    as.data.frame() %>%
    
    gather(., var_names, var_values, 
           -sport, -league, -match_date, -is_home, -team, -gp_suffix) %>%
    rowwise() %>%
    mutate(var_names_new = paste(var_names, "_", gp_suffix, sep = "")) %>%
    select(-var_names, -gp_suffix) %>%
    spread(., var_names_new, var_values) %>%
    as.data.frame()
  
  lapply(c(1:length(cluster)), function(x) cluster[[x]]$close())
  rm(cluster)
  
  return(test_league)
}

modelling_data <- get_modellingdata(test %>% filter(league %in% "E0"))
