library(tidyr)
library(readr)
library(dplyr)

wide <- read_csv('data/covid-19-ma-county-wide.csv')

long <- wide %>%
  gather(date, total_cases, 
         -`County`, -`State`, -Lat, -Long) %>%
  mutate(
    date=as.Date(date, "%m/%d/%Y"),
    new_cases = total_cases - lag(total_cases)
    ) %>%
  write_csv('data/covid-19-ma-county-long.csv', na='')



