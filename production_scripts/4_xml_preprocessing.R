# ----- Pre-processing the XML Feed Data -----

# ---- Library Loading -----
library(dplyr)
library(data.table)

# ----- Set Working Directory -----
setwd("C:/Users/Peter.Tomko/OneDrive - 4Finance/concept/Betting Data Science")

# ----- Watched Leagues -----
football_leagues <-
  c("1. anglická liga",
    "2. anglická liga",
    "3. anglická liga",
    "4. anglická liga",
    "5. anglická liga",
    
    "1. skotská liga",
    "2. skotská liga",
    "3. skotská liga",
    "4. skotská liga",
    
    "1. německá liga",
    "2. německá liga",
    
    "1. italská liga",
    "2. italská liga",
    
    "1. španělská liga",
    "2. španělská liga",
    
    "1. francouzská liga",
    "2. francouzská liga",
    
    "1. nizozemská liga",
    
    "1. dánská liga",
    
    "1. portugalská liga",
    
    "1. turecká liga",
    
    "1. řecká liga")

# ----- Loading the Data -----
data_xml <- readRDS("data/production_data/1_tipsport_feedxml.RData")
master_data <- readRDS(file = "data/production_data/0_data_download.RData")

# ----- Select only specific leagues and two types of bets - win/loss, +/-2.5 ----
data_subset <- data_xml %>%
  filter(sport_category %in% "Fotbal - muži" &
           (eventtypedescription %in% c("Vítěz", "Počet gólů (bodů)")) &
           sport_league %in% football_leagues & gamepart %in% "1/1" & 
           !(type %in% "x") & fullname %in% c("Méně než 2.5", "Více než 2.5")) %>%
  rbind(., data_xml %>%
          filter(sport_category %in% "Fotbal - muži" &
                   eventtypedescription %in% c("Vítěz") &
                   sport_league %in% football_leagues & gamepart %in% "1/1" & 
                   !(type %in% "x")))

# ----- Synchronizing the team Names -----
mapping_table <- master_data %>%
  select(Div, HomeTeam) %>%
  rename(team_input = HomeTeam) %>%
  rbind(., master_data %>%
          select(Div, AwayTeam) %>%
          rename(team_input = AwayTeam)) %>%
  rowwise() %>%
  mutate(team_input = trimws(team_input, which = "both")) %>%
  distinct()

mapping_table <- mapping_table %>%
  left_join(., data_subset %>% 
              select(fullname) %>% 
              distinct() %>% 
              mutate(correct = 1) %>% 
              rename(team_input = fullname))

mapping_table$fullname <- NA
mapping_table$correct[is.na(mapping_table$correct)] <- 0
mapping_table$fullname[mapping_table$correct == 1] <- 
  mapping_table$team_input[mapping_table$correct == 1]

# - English League: E
mapping_table$fullname[mapping_table$team_input %in% "Man City"] <- "Manchester C."
mapping_table$fullname[mapping_table$team_input %in% "Man United"] <- "Manchester U."
mapping_table$fullname[mapping_table$team_input %in% "Crystal Palace"] <- "Crystal P."
mapping_table$fullname[mapping_table$team_input %in% "Sheffield United"] <- "Sheffield Utd."
mapping_table$fullname[mapping_table$team_input %in% "Nott'm Forest"] <- "Nottingham"
mapping_table$fullname[mapping_table$team_input %in% "Bristol City"] <- "Bristol C."
mapping_table$fullname[mapping_table$team_input %in% "Cardiff"] <- "Cardiff City"
mapping_table$fullname[mapping_table$team_input %in% "QPR"] <- "Q.P.R."
mapping_table$fullname[mapping_table$team_input %in% "Sheffield Weds"] <- "Sheffield W."
mapping_table$fullname[mapping_table$team_input %in% "West Brom"] <- "W. B. Albion"
mapping_table$fullname[mapping_table$team_input %in% "Middlesboro"] <- "Middlesbrough"
mapping_table$fullname[mapping_table$team_input %in% "Wimbledon"] <- "AFC Wimbledon"
mapping_table$fullname[mapping_table$team_input %in% "AFC Telford United"] <- "Telford"
mapping_table$fullname[mapping_table$team_input %in% "Alfreton Town"] <- "Alfreton"
mapping_table$fullname[mapping_table$team_input %in% "Altrincham"] <- "Altrincham"
mapping_table$fullname[mapping_table$team_input %in% "Bath City"] <- "Bath City"
mapping_table$fullname[mapping_table$team_input %in% "Boston"] <- "Boston Utd."
mapping_table$fullname[mapping_table$team_input %in% "Bristol Rvs"] <- "Bristol Rovers"
mapping_table$fullname[mapping_table$team_input %in% "Braintree Town"] <- "Braintree"
mapping_table$fullname[mapping_table$team_input %in% "Boreham Wood"] <- "Boreham"
mapping_table$fullname[mapping_table$team_input %in% "Burton"] <- "Burton"
mapping_table$fullname[mapping_table$team_input %in% "York"] <- "York"
mapping_table$fullname[mapping_table$team_input %in% "Weymouth"] <- "Weymouth"
mapping_table$fullname[mapping_table$team_input %in% "Crawley Town"] <- "Crawley"
mapping_table$fullname[mapping_table$team_input %in% "Darlington"] <- "Darlington"
mapping_table$fullname[mapping_table$team_input %in% "Dartford"] <- "Dartford"
mapping_table$fullname[mapping_table$team_input %in% "Fleetwood Town"] <- "Fleetwood"
mapping_table$fullname[mapping_table$team_input %in% "Leyton Orient"] <- "Leyton"
mapping_table$fullname[mapping_table$team_input %in% "Lincoln"] <- "Lincoln"
mapping_table$fullname[mapping_table$team_input %in% "Dover Athletic"] <- "Dover Athletic"
mapping_table$fullname[mapping_table$team_input %in% "Eastbourne Borough"] <- "Eastbourne Borough"
mapping_table$fullname[mapping_table$team_input %in% "Hereford"] <- "Hereford"
mapping_table$fullname[mapping_table$team_input %in% "Peterboro"] <- "Peterborough"
mapping_table$fullname[mapping_table$team_input %in% "Newport County"] <- "Newport"
mapping_table$fullname[mapping_table$team_input %in% "Dag and Red"] <- "Dagenham"
mapping_table$fullname[mapping_table$team_input %in% "Notts County"] <- "Notts"
mapping_table$fullname[mapping_table$team_input %in% "Maidstone"] <- "Maidstone United"
mapping_table$fullname[mapping_table$team_input %in% "Gateshead"] <- "Gateshead"
mapping_table$fullname[mapping_table$team_input %in% "Havant & Waterlooville"] <- "Havant"
mapping_table$fullname[mapping_table$team_input %in% "Guiseley"] <- "Guiseley"
mapping_table$fullname[mapping_table$team_input %in% "Chester"] <- "Chester"
mapping_table$fullname[mapping_table$team_input %in% "Southport"]	<- "Southport"
mapping_table$fullname[mapping_table$team_input %in% "Kidderminster"]	<- "Kiderminster"
mapping_table$fullname[mapping_table$team_input %in% "Welling United"] <- "Welling"
mapping_table$fullname[mapping_table$team_input %in% "Telford United"] <- "Telford"
mapping_table$fullname[mapping_table$team_input %in% "Kettering Town"] <- "Kettering"
mapping_table$fullname[mapping_table$team_input %in% "Farsley"]	<- "Farsley"
mapping_table$fullname[mapping_table$team_input %in% "St. Albans"] <- "St. Albans City"
mapping_table$fullname[mapping_table$team_input %in% "Kidderminster"] <- "Kidderminster"

# - Spain League: SP
mapping_table$fullname[mapping_table$team_input %in% "Granada 74"] <- "Granada"
mapping_table$fullname[mapping_table$team_input %in% "Alaves"] <- "Alavés"
mapping_table$fullname[mapping_table$team_input %in% "Alcorcon"] <- "Alcorcón"
mapping_table$fullname[mapping_table$team_input %in% "Almeria"] <- "Almería"
mapping_table$fullname[mapping_table$team_input %in% "Ath Bilbao"] <- "Bilbao"
mapping_table$fullname[mapping_table$team_input %in% "Ath Madrid"] <- "Atlético Madrid"
mapping_table$fullname[mapping_table$team_input %in% "Barcelona"] <- "FC Barcelona"
mapping_table$fullname[mapping_table$team_input %in% "Betis"] <- "Bettis Sevilla"
mapping_table$fullname[mapping_table$team_input %in% "Cadiz"] <- "Cádiz"
mapping_table$fullname[mapping_table$team_input %in% "Celta"] <- "Vigo"
mapping_table$fullname[mapping_table$team_input %in% "Espanol"] <- "Espanyol"
mapping_table$fullname[mapping_table$team_input %in% "Extremadura UD"] <- "Extremadura"
mapping_table$fullname[mapping_table$team_input %in% "Hercules"] <- "Heracles"
mapping_table$fullname[mapping_table$team_input %in% "La Coruna"] <- "La Coruňa"
mapping_table$fullname[mapping_table$team_input %in% "Leganes"] <- "Leganés"
mapping_table$fullname[mapping_table$team_input %in% "Malaga"] <- "Málaga"
mapping_table$fullname[mapping_table$team_input %in% "Mirandes"] <- "Mirandés"
mapping_table$fullname[mapping_table$team_input %in% "Santander"] <- "Racing Santander"
mapping_table$fullname[mapping_table$team_input %in% "Alaves"] <- "Alavés"
mapping_table$fullname[mapping_table$team_input %in% "Sevilla"] <- "FC Sevilla"
mapping_table$fullname[mapping_table$team_input %in% "Sp Gijon"] <- "Gijón"
mapping_table$fullname[mapping_table$team_input %in% "U.Las Palmas"] <- "Las Palmas"
mapping_table$fullname[mapping_table$team_input %in% "Villareal"] <- "Villarreal"

# - Italian League: I
mapping_table$fullname[mapping_table$team_input %in% "Roma"] <- "AS Roma"
mapping_table$fullname[mapping_table$team_input %in% "Verona"] <- "Hellas Verona"
mapping_table$fullname[mapping_table$team_input %in% "Inter"] <- "Inter Milano"
mapping_table$fullname[mapping_table$team_input %in% "Milan"] <- "AC Milan"
mapping_table$fullname[mapping_table$team_input %in% "Lazio"] <- "Lazio Roma"
mapping_table$fullname[mapping_table$team_input %in% "Atalanta"] <- "Bergamo"
mapping_table$fullname[mapping_table$team_input %in% "Genoa"] <- "FC Genoa"
mapping_table$fullname[mapping_table$team_input %in% "Torino"] <- "FC Torino"

# - French League: F
mapping_table$fullname[mapping_table$team_input %in% "Ajaccio"] <- "AC Ajaccio"
mapping_table$fullname[mapping_table$team_input %in% "Ajaccio GFCO"] <- "AC Ajaccio"
mapping_table$fullname[mapping_table$team_input %in% "Auxerre"] <- "Auxerre"
mapping_table$fullname[mapping_table$team_input %in% "Caen"] <- "Caen"
mapping_table$fullname[mapping_table$team_input %in% "Guingamp"] <- "Guingamp"
mapping_table$fullname[mapping_table$team_input %in% "Chateauroux"] <- "Chateauroux"
mapping_table$fullname[mapping_table$team_input %in% "Le Havre"] <- "Le Havre"
mapping_table$fullname[mapping_table$team_input %in% "Le Mans"] <- "Le Mans"
mapping_table$fullname[mapping_table$team_input %in% "Lille"] <- "Lille"
mapping_table$fullname[mapping_table$team_input %in% "Lens"] <- "Lens"
mapping_table$fullname[mapping_table$team_input %in% "Sochaux"] <- "Sochaux"
mapping_table$fullname[mapping_table$team_input %in% "Valenciennes"] <- "Valenciennes"
mapping_table$fullname[mapping_table$team_input %in% "Troyes"] <- "Troyes"
mapping_table$fullname[mapping_table$team_input %in% "St Etienne"] <- "St. Etienne"
mapping_table$fullname[mapping_table$team_input %in% "Nancy"] <- "Nancy"
mapping_table$fullname[mapping_table$team_input %in% "Grenoble"] <- "Grenoble"
mapping_table$fullname[mapping_table$team_input %in% "Paris FC"] <- "Paris FC"

# - German League: D
mapping_table$fullname[mapping_table$team_input %in% "Bayern Munich"] <- "Bayern München"
mapping_table$fullname[mapping_table$team_input %in% "Dresden"] <- "Dynamo Dresden"
mapping_table$fullname[mapping_table$team_input %in% "Dusseldorf"] <- "Düsseldorf"
mapping_table$fullname[mapping_table$team_input %in% "Ein Frankfurt"] <- "Frankfurt"
mapping_table$fullname[mapping_table$team_input %in% "Dusseldorf"] <- "Düsseldorf"
mapping_table$fullname[mapping_table$team_input %in% "F Koln"] <- "1 FC. Köln"
mapping_table$fullname[mapping_table$team_input %in% "FC Koln"] <- "1 FC. Köln"
mapping_table$fullname[mapping_table$team_input %in% "Fortuna Dusseldorf"] <- "Düsseldorf"
mapping_table$fullname[mapping_table$team_input %in% "Leipzig"] <- "RB Leipzig"
mapping_table$fullname[mapping_table$team_input %in% "M'gladbach"] <- "Monchengladbach"
mapping_table$fullname[mapping_table$team_input %in% "M'Gladbach"] <- "Monchengladbach"
mapping_table$fullname[mapping_table$team_input %in% "Nurnberg"] <- "Nürnberg"
mapping_table$fullname[mapping_table$team_input %in% "St Pauli"] <- "St. Pauli"
mapping_table$fullname[mapping_table$team_input %in% "Schalke 04"] <- "Schalke"
mapping_table$fullname[mapping_table$team_input %in% "Werder Bremen"] <- "Bremen"

# - Scotland League: SC
mapping_table$fullname[mapping_table$team_input %in% "Ayr"] <- "Ayr Utd."
mapping_table$fullname[mapping_table$team_input %in% "Dundee United"] <- "Dundee Utd."
mapping_table$fullname[mapping_table$team_input %in% "Dundee"] <- "Dundee FC"
mapping_table$fullname[mapping_table$team_input %in% "Queen of Sth"] <- "Queen South"
mapping_table$fullname[mapping_table$team_input %in% "Airdrie Utd"] <- "Airdrie"
mapping_table$fullname[mapping_table$team_input %in% "Albion Rvs"] <- "Albion Rovers"
mapping_table$fullname[mapping_table$team_input %in% "Annan Athletic"] <- "Annan Ath."
mapping_table$fullname[mapping_table$team_input %in% "Raith Rvs"] <- "Raith Rovers"
mapping_table$fullname[mapping_table$team_input %in% "St Mirren"] <- "St. Mirren"
mapping_table$fullname[mapping_table$team_input %in% "St Johnstone"] <- "St. Johnstone"
mapping_table$fullname[mapping_table$team_input %in% "Stirling"] <- "Stirling Albion"

saveRDS(object = mapping_table %>% as.data.frame(), 
        file = "data/production_data/4_xml_preprocessing.RData")
