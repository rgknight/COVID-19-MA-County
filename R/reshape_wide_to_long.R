library(tidyr)
library(readr)
library(dplyr)
library(jsonlite)

wide <- read_csv('data/covid-19-ma-county-wide.csv')

# Reshape to long format, suitable for Tableau / R
long <- wide %>%
  gather(date, total_cases, 
         -`County`, -`State`, -Lat, -Long) %>%
  mutate(
    date=as.Date(date, "%m/%d/%Y")
  ) %>%
  arrange(County, date) %>%
  mutate(
    new_cases = total_cases - lag(total_cases),
    percent_growth = round(100*(new_cases / lag(total_cases)),2)
    ) %>%
  write_csv('data/covid-19-ma-county-long.csv', na='')

# Create json
# Leaving out the county-level info bc it's R to manipulate in R
no_county_data <- long %>%
  select(-Lat, -Long, -State)

json <- split(no_county_data[ , -1], no_county_data$County) %>%
  toJSON(prety=TRUE) %>%
  write_json('data/covid-19-ma-county.json')
