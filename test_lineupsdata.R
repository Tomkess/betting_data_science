library(data.table)
library(dplyr)
library(tidyverse)
library(xml2)
library(rvest)

weburl <- "http://www.11v11.com/competitions/premier-league/2016/matches/"
links_all <- 
  read_html(weburl) %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset(., "/matches/") %>%
  str_subset(., "/competitions/", negate = T) %>%
  unique()

match_url <- "https://www.11v11.com/matches/manchester-united-v-afc-bournemouth-17-may-2016-319192/"
data_all <- 
  read_html(match_url) %>%
  html_nodes(xpath = '//*[@class="lineup"]')

# - home
temp <- data_all[[1]] %>%
  html_nodes(xpath = '//*[@class="home"]')

temp[[3]] %>%
  html_nodes("a") %>%
  html_attr("href")

temp[[3]] %>%
  html_nodes("a") %>%
  str_replace_all(., "</a", "") %>%
  str_split(., '>')
