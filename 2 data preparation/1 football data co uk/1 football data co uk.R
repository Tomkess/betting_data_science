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
  str_subset("\\m.php") %>%
  str_subset(string = ., pattern = "https://") %>%
  str_subset(string = ., pattern = "scam", negate = TRUE) %>%
  str_subset(string = ., pattern = "betting", negate = TRUE) %>%
  str_subset(string = ., pattern = "downloadm", negate = TRUE) %>%
  str_subset(string = ., pattern = "gambling", negate = TRUE) %>%
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
              as.data.frame()
            
            return(data)})) %>%
  
  # - get season and league
  rowwise() %>%
  dplyr::mutate(
    season = str_split(file_link, "/")[[1]][2],
    league = str_replace(str_split(file_link, "/")[[1]][3], ".csv", "")
  ) %>%
  dplyr::select(-name_league, -file_link) %>%
  as.data.frame()

# ----- Preprocess Files -----
master_data <- 
  
  master_data %>%
  
  dplyr::group_by(file_link) %>%
  dplyr::mutate(
    prep_data = 
      map(orig_data, 
          function(i_data) {
            
            data <- 
              
              i_data %>%
              as.data.frame() %>%
              
              # - remove unimportant rows
              dplyr::filter(FTR %in% c("H", "D", "A")) %>%
              
              # - Home Team and Away Team (some .csv files have AT and HT)
              rowwise() %>%
              
              dplyr::mutate(
                HomeTeam = ifelse("HT" %in% names(.), HT, HomeTeam),
                AwayTeam = ifelse("AT" %in% names(.), AT, AwayTeam)
              ) %>%
              
              # - create standardized date column (created_at - match date)
              rowwise() %>%
              dplyr::mutate(
                get_day = str_split(Date, "/")[[1]][1],
                get_month = str_split(Date, "/")[[1]][2],
                get_year = str_split(Date, "/")[[1]][3],
                year_adj = 
                  ifelse(get_year %in% c("93", "94", "95", "96", 
                                         "97", "98", "99"), 
                         paste("19", get_year, sep = ""), 
                         get_year)) %>%
              
              dplyr::mutate(
                year_adj = ifelse(str_length(year_adj) < 3, 
                                  paste("20", get_year, sep = ""), year_adj)
              ) %>%
              dplyr::mutate(
                created_at = as.Date(paste(get_day, get_month, year_adj, 
                                           sep = "/"), format = "%d/%m/%Y")
              ) %>%
              distinct() %>%
              
              # - ensure the columns exist
              rowwise() %>%
              dplyr::mutate(
                FTR = ifelse("FTR" %in% names(.), FTR, NA), 
                FTHG = ifelse("FTHG" %in% names(.), FTHG, NA),
                FTAG = ifelse("FTAG" %in% names(.), FTAG, NA), 
                HS = ifelse("HS" %in% names(.), HS, NA), 
                AS = ifelse("AS" %in% names(.), AS, NA), 
                HST = ifelse("HST" %in% names(.), HST, NA), 
                AST = ifelse("AST" %in% names(.), AST, NA), 
                HF = ifelse("HF" %in% names(.), HF, NA), 
                AF = ifelse("AF" %in% names(.), AF, NA), 
                HC = ifelse("HC" %in% names(.), HC, NA), 
                AC = ifelse("AC" %in% names(.), AC, NA), 
                HY = ifelse("HY" %in% names(.), HY, NA), 
                AY = ifelse("AY" %in% names(.), AY, NA), 
                HR = ifelse("HR" %in% names(.), HR, NA), 
                AR = ifelse("AR" %in% names(.), AR, NA),
                B365H = ifelse("B365H" %in% names(.), B365H, NA), 
                B365D = ifelse("B365D" %in% names(.), B365D, NA), 
                B365A = ifelse("B365A" %in% names(.), B365A, NA),
                BWH = ifelse("BWH" %in% names(.), BWH, NA), 
                BWD = ifelse("BWD" %in% names(.), BWD, NA), 
                BWA = ifelse("BWA" %in% names(.), BWA, NA),
                IWH = ifelse("IWH" %in% names(.), IWH, NA), 
                IWD = ifelse("IWD" %in% names(.), IWD, NA), 
                IWA = ifelse("IWA" %in% names(.), IWA, NA),
                PSH = ifelse("PSH" %in% names(.), PSH, NA), 
                PSD = ifelse("PSD" %in% names(.), PSD, NA), 
                PSA = ifelse("PSA" %in% names(.), PSA, NA),
                WHH = ifelse("WHH" %in% names(.), WHH, NA), 
                WHD = ifelse("WHD" %in% names(.), WHD, NA), 
                WHA = ifelse("WHA" %in% names(.), WHA, NA),
                VCH = ifelse("VCH" %in% names(.), VCH, NA), 
                VCD = ifelse("VCD" %in% names(.), VCD, NA), 
                VCA = ifelse("VCA" %in% names(.), VCA, NA),
                B365.2.5 = ifelse("B365.2.5" %in% names(.), 
                                  `B365.2.5`, NA), 
                B365.2.5.1 = ifelse("B365.2.5.1" %in% names(.), 
                                    `B365.2.5.1`, NA), 
                P.2.5 = ifelse("P.2.5" %in% names(.), `P.2.5`, NA), 
                P.2.5.1 = ifelse("P.2.5.1" %in% names(.), 
                                 `P.2.5.1`, NA), 
                GB.2.5 = 
                  ifelse("GB.2.5" %in% names(.), `GB.2.5`, NA), 
                GB.2.5.1 = ifelse("GB.2.5.1" %in% names(.), 
                                  `GB.2.5.1`, NA)) %>%
              
              # - select relevant columns
              dplyr::select(created_at, HomeTeam, AwayTeam, 
                            FTR, FTHG, FTAG, 
                            HS, AS, HST, AST, HF, AF, HC, AC, HY, AY, HR, AR,
                            B365H, B365D, B365A, BWH, BWD, BWA, 
                            IWH, IWD, IWA, PSH, PSD, PSA, 
                            WHH, WHD, WHA, VCH, VCD, VCA,
                            B365.2.5, B365.2.5.1, P.2.5, P.2.5.1, 
                            GB.2.5, GB.2.5.1) %>%
              distinct() %>%
              
              # - create match id
              dplyr::group_by(created_at) %>%
              dplyr::mutate(match_id = row_number()) %>%
              as.data.frame() %>%
              
              # - get numeric values at selected columns
              dplyr::mutate_at(vars(-created_at, -HomeTeam, -AwayTeam, -FTR), 
                               as.numeric) %>%
              
              # - gather by team and is_home
              tidyr::gather(., is_home, team, 
                            -created_at, -match_id, -FTR,
                            
                            # - Match Statistics
                            -FTHG, -FTAG, -HS, -AS, -HST, -AST, -HF, -AF, 
                            -HC, -AC, -HY, -AY, -HR, -AR,
                            
                            # - Bet Odds
                            -B365H, -B365D, -B365A, -BWH, -BWD, -BWA, 
                            -IWH, -IWD, -IWA, -PSH, -PSD, -PSA, 
                            -WHH, -WHD, -WHA, -VCH, -VCD, -VCA,
                            -B365.2.5, -B365.2.5.1, 
                            -P.2.5, -P.2.5.1, 
                            -GB.2.5, -GB.2.5.1) %>%
              
              # - dummy is_home
              dplyr::mutate(
                is_home = recode(is_home, "HomeTeam" = 1, "AwayTeam" = 0)
              ) %>%
              as.data.frame()
            
            return(data)
            }))

# ----- Match Calendar -----
t_match_calendar <- 
  
  master_data %>%
  
  dplyr::group_by(file_link) %>%
  dplyr::mutate(cal_data = map(prep_data, function(i_data){
    
    temp <- 
      i_data %>%
      
      # - get Away Team
      dplyr::select(created_at, team, match_id, is_home) %>%
      dplyr::filter(is_home == 0) %>%
      dplyr::rename(AwayTeam = team) %>%
      dplyr::select(-is_home) %>%
      distinct() %>%
      as.data.frame() %>%
      
      # - join Home Team
      left_join(., i_data %>%
                  dplyr::select(created_at, team, match_id, is_home) %>%
                  dplyr::filter(is_home == 1) %>%
                  dplyr::rename(HomeTeam = team) %>%
                  dplyr::select(-is_home) %>%
                  distinct() %>%
                  as.data.frame()) %>% 
      as.data.frame()
    
    return(temp)
  })) %>%
  dplyr::select(season, league, cal_data) %>%
  unnest(c(cal_data)) %>%
  distinct() %>% 
  
  dplyr::select(-match_id) %>%
  as.data.frame() %>%
  
  # - create index column
  dplyr::mutate(match_id = row_number()) %>%
  dplyr::rename(home_t = HomeTeam,
                away_t = AwayTeam) %>%
  as.data.frame()

# ----- Match Stats -----
t_match_stats <- 
  
  master_data %>%
  as.data.frame() %>% 
  
  dplyr::select(season, league, prep_data) %>%
  unnest(c(prep_data)) %>%
  as.data.frame() %>%

  dplyr::rename_at(names(.), tolower) %>% 
  as.data.frame()

# ----- Writing Table to Database -----
if(nrow(t_match_calendar) > 0){
  betting_ds$evalSQL(sql = "drop table if exists t_match_calendar cascade;")
  betting_ds$writeTable(df = t_match_calendar, 
                        table_name = "t_match_calendar", 
                        is_append = F, is_overwrite = T)
  
}

if(nrow(t_match_stats) > 0){
  betting_ds$evalSQL(sql = "drop table if exists t_match_stats cascade;")
  betting_ds$writeTable(df = t_match_stats, 
                        table_name = "t_match_stats", 
                        is_append = F, is_overwrite = T)
  
}

# ----- Download Other Data -----
source("C:/Users/Peter/Desktop/ds_projects/betting_data_science/2 data preparation/1 football data co uk/2 football data co uk - other leagues.R")