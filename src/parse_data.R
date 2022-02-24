library(rvest)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(tidyr)

poses_page <- read_html("data/romwod-app-poses.html")

json_data <- poses_page %>% 
  html_elements("script") %>% 
  nth(6) %>% 
  html_text() %>% 
  str_remove("\n              window.__PRELOADED_STATE__ = ") %>% 
  str_trim() %>% 
  fromJSON(simplifyVector = FALSE)

poses <- json_data %>% 
  pluck("poses")

poses %>% 
  write_json("data/poses.json")
