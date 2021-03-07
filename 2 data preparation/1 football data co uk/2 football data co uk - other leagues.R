library("generalToolboxR")

# ----- Download t_match_calendar -----
betting_ds <- dbManager$new()
betting_ds$db_config(db_drv = "PostgreSQL",
                     db_name = "betting_ds",
                     host = "localhost",
                     port = 5432,
                     user = "postgres",
                     password = "thatSounds77")

# ----- Get links to website with .csv files -----
links_all <- 
  read_html("https://www.football-data.co.uk/data.php") %>%
  
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\.php") %>%
  str_subset(string = ., pattern = "https://") %>%
  str_subset(string = ., pattern = "scam", negate = TRUE) %>%
  str_subset(string = ., pattern = "betting", negate = TRUE) %>%
  str_subset(string = ., pattern = "downloadm", negate = TRUE) %>%
  str_subset(string = ., pattern = "gambling", negate = TRUE) %>%
  str_subset(string = ., pattern = "blog", negate = TRUE) %>%
  str_subset(string = ., pattern = "resources", negate = TRUE) %>%
  str_subset(string = ., pattern = "all_new", negate = TRUE) %>%
  str_subset(string = ., pattern = "matches", negate = TRUE) %>%
  str_subset(string = ., pattern = "freebets", negate = TRUE) %>%
  str_subset(string = ., pattern = "pinnacle", negate = TRUE) %>%
  str_subset(string = ., pattern = "books", negate = TRUE) %>%
  str_subset(string = ., pattern = "disclaimer", negate = TRUE) %>%
  str_subset(string = ., pattern = "link", negate = TRUE) %>%
  str_subset(string = ., pattern = "contact", negate = TRUE) %>%
  str_subset(string = ., pattern = "data.php", negate = TRUE) %>%
  str_subset(string = ., pattern = "m.php", negate = TRUE) %>%
  str_subset(string = ., pattern = "argentia", negate = TRUE) %>%
  unique()

# ----- Get file links -----
links_data <- 
  data.frame(name_league = links_all) %>%
  
  dplyr::group_by(name_league) %>%
  dplyr::mutate(file_link = 
                  map(name_league, function(i_link){
                    
                    data.frame(file_link = 
                                 html(i_link) %>%
                                 html_nodes("a") %>%
                                 html_attr("href") %>%
                                 str_subset("\\.csv") %>%
                                 as.character())
                  })) %>%
  tidyr::unnest(c(file_link)) %>%
  as.data.frame() %>% 
  
  distinct() %>% 
  as.data.frame()

# ----- Download Files -----
master_data <- 
  
  links_data %>%
  
  dplyr::group_by(file_link) %>%
  dplyr::mutate(
    orig_data = 
      map(file_link, 
          function(i_link) {
            
            data <- 
              read.csv(paste("https://www.football-data.co.uk/", 
                             i_link, sep = ""), stringsAsFactors = FALSE) %>%
              as.data.frame() %>% 
              
              dplyr::mutate(Season = as.character(Season),
                            League = trimws(League, which = "both"),
                            Country = trimws(Country, which = "both")) %>% 
              as.data.frame()
            
            return(data)
          }
      )
  ) %>% 
  
  tidyr::unnest(c(orig_data)) %>% 
  
  # create match id
  dplyr::group_by(name_league, Season, Country, Date) %>% 
  dplyr::mutate(match_id = row_number()) %>% 
  as.data.frame() %>% 
  
  # renaming
  dplyr::rename(season = Season,
                league = League,
                away_t = Away,
                home_t = Home,
                created_at = Date,
                ftr = Res,
                fthg = HG,
                ftag = AG,
                
                psh = PH,
                psa = PA,
                psd = PD) %>% 
  
  # formatting
  dplyr::mutate(created_at = lubridate::dmy(created_at),
                league = paste(league, " - ", Country, sep = "")) %>% 
  as.data.frame() %>% 
  
  # exclude
  dplyr::select(-Country, -Time, 
                -AvgH, -AvgA, -AvgD, 
                -MaxH, -MaxA, -MaxD) %>% 
  as.data.frame()

t_match_calendar <- 
  master_data %>% 
  dplyr::select(file_link, season, league, created_at, 
                away_t, home_t, match_id) %>% 
  as.data.frame()

t_match_stats <- 
  
  master_data %>% 
  dplyr::select(-file_link, -name_league) %>% 
  as.data.frame() %>% 
  
  # - gather by team and is_home
  tidyr::gather(., is_home, team, 
                -created_at, -match_id, -ftr,
                -league, -season,
                
                # - Match Statistics
                -fthg, -ftag,
                
                # - Bet Odds
                -psh, -psd, -psa) %>%
  
  # - dummy is_home
  dplyr::mutate(
    is_home = recode(is_home, "home_t" = 1, "away_t" = 0)
  ) %>%
  as.data.frame() %>% 
  
  dplyr::mutate(
    hs = NA,
    `as` = NA,
    hst = NA,
    ast = NA,
    hf = NA,
    af = NA,
    hc = NA,
    ac = NA,
    hy = NA,
    ay = NA,
    hr = NA,
    ar = NA,
    b365h = NA,
    b365d = NA,
    b365a = NA,
    bwh = NA,
    bwd = NA,
    bwa = NA,
    iwh = NA,
    iwd = NA,
    iwa = NA,
    whh = NA,
    whd = NA,
    wha = NA,
    vch = NA,
    vcd = NA,
    vca = NA,
    `b365.2.5` = NA,
    `b365.2.5.1` = NA,
    `p.2.5` = NA,
    `p.2.5.1` = NA,
    `gb.2.5` = NA,
    `gb.2.5.1` = NA
  ) %>% 
  as.data.frame()

# ----- Writing Table to Database -----
if(nrow(t_match_calendar) > 0){
  betting_ds$writeTable(df = t_match_calendar, 
                        table_name = "t_match_calendar", 
                        is_append = T, is_overwrite = F)
  
}

if(nrow(t_match_stats) > 0){
  betting_ds$writeTable(df = t_match_stats, 
                        table_name = "t_match_stats", 
                        is_append = T, is_overwrite = F)
  
}
