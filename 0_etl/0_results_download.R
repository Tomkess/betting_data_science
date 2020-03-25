# ----- Destination File Set up -----
dest_path <- "0_etl/db_temp/0_results_download.RData"

# ----- Initial Webpage crawling -----
start_url <- c("https://www.football-data.co.uk/data.php")
start_webpage <- html(start_url)

links_all <- start_webpage %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("\\m.php") %>%
  str_subset(string = ., pattern = "https://") %>%
  str_subset(string = ., pattern = "scam", negate = TRUE) %>%
  str_subset(string = ., pattern = "betting", negate = TRUE) %>%
  str_subset(string = ., pattern = "downloadm", negate = TRUE) %>%
  str_subset(string = ., pattern = "gambling", negate = TRUE)

# ----- Get file links -----
current_data <- lapply(links_all, 
                       function(x){ 
                         page <- html(x)
                         return(page %>%
                                  html_nodes("a") %>%
                                  html_attr("href") %>%
                                  str_subset("\\.csv"))})

names(current_data) <- links_all
links_data <- data.frame(name_league = rep(names(current_data), 
                                           sapply(current_data, length)),
                         file_link = unlist(current_data))

if("0_results_download.RData" %in% list.files("0_etl/db_temp")){
  links_data <- links_data %>%
    filter(str_detect(string = file_link, pattern = "1920") == TRUE) %>%
    as.data.frame()
}else{
  links_data <- links_data %>%
    as.data.frame()
}

# ----- Download Files -----
masterdata_list <- lapply(links_data$file_link, 
                          function(x) 
                            read.csv(paste("https://www.football-data.co.uk/", 
                                           x, sep = ""), 
                                     stringsAsFactors = FALSE))

# ----- Data Preprocessing -----
asian_handicap <- c("BbAH", "BbAHh", "AHh", "BbMxAHH", "BbAvAHH", 
                    "BbMxAHA", "BbAvAHA", "GBAHH", "GBAHA", "GBAH", "LBAHH", 
                    "LBAHA", "LBAH", "B365AHH", "B365AHA", "B365AH", "PAHH",
                    "PAHA", "MaxAHH", "MaxAHA", "AvgAHH", "AvgAHA")
unimportant_columns <- c("Bb1X2", "BbMxH", "BbAvH", "BbMxD", "BbAvD", 
                         "BbMxA", "BbAvA", "BbOU", "AHCh", "B365CAHH", 
                         "B365CAHA", "PCAHH", "PCAHA", "MaxCAHH", 
                         "MaxCAHA", "AvgCAHH", "AvgCAHA", "BbMx.2.5", 
                         "BbAv.2.5", "BbMx.2.5.1", "BbAv.2.5.1", "Attendance", 
                         "X.2", "X.3", "X.4", "LB", "LB.1", "LB.2", "BbAHh")

master_data <- rbindlist(masterdata_list, fill = TRUE, use.names = TRUE)
master_data <- master_data %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  distinct() %>%
  select(-one_of(c("Time", "Referee", "HTHG", "HTAG", "HTR"))) %>%
  filter(FTR %in% c("H", "D", "A")) %>%
  rowwise() %>%
  mutate(PSCH = as.numeric(PSCH)) %>%
  select(-one_of(c(asian_handicap))) %>%
  select(-one_of(c(unimportant_columns))) %>%
  rowwise() %>%
  mutate(HomeTeam = 
           ifelse("HT" %in% names(.), coalesce(HomeTeam, HT), HomeTeam),
         AwayTeam = 
           ifelse("AT" %in% names(.), coalesce(AwayTeam, AT), AwayTeam)) %>%
  select(-one_of(c("HT", "AT"))) %>%
  mutate_at(vars(-Div, -Date, -HomeTeam, -AwayTeam, -FTR), as.numeric) %>%
  rowwise() %>%
  mutate(get_day = str_split(Date, pattern = "/")[[1]][1],
         get_month = str_split(Date, pattern = "/")[[1]][2],
         get_year = str_split(Date, pattern = "/")[[1]][3]) %>%
  rowwise() %>%
  mutate(year_adj = 
           ifelse(get_year %in% c("93", "94", "95", "96", "97", "98", "99"), 
                  paste("19", get_year, sep = ""), get_year)) %>%
  rowwise() %>%
  mutate(year_adj = 
           ifelse(str_length(year_adj) < 3, 
                  paste("20", get_year, sep = ""), year_adj)) %>%
  rowwise() %>%
  mutate(created_at = as.Date(paste(get_day, get_month, year_adj, sep = "/"), 
                              format = "%d/%m/%Y")) %>%
  select(-one_of(c("year_adj", "get_year", "get_month", "get_day", "Date"))) %>%
  distinct()

master_data_actual <- master_data
rm(master_data)

# ----- Save RDS file -----
if("0_results_download.RData" %in% list.files("0_etl/db_temp")){
  load(dest_path)
  
  data_save <- 
    bind_rows(master_data_actual, master_data) %>%
    as.data.frame()  %>%
    group_by(.) %>%
    distinct() %>%
    rowwise() %>%
    mutate(HomeTeam = trimws(HomeTeam, which = "both"),
           AwayTeam = trimws(AwayTeam, which = "both"))
  
  master_data <- data_save
  save(master_data, file = dest_path)
  
}else{
  
  master_data_actual <-
    master_data_actual %>%
    as.data.frame() %>%
    group_by(.) %>%
    distinct() %>%
    rowwise() %>%
    mutate(HomeTeam = trimws(HomeTeam, which = "both"),
           AwayTeam = trimws(AwayTeam, which = "both"))
  
  master_data <- master_data_actual
  save(master_data, file = dest_path)
  
}
