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

long %>% filter(date > as.Date("2020-03-10")) %>% View()

# Create json
# Leaving out the county-level info bc it's R to manipulate in R
no_county_data <- long %>%
  select(-Lat, -Long, -State)

json_list <- list()
json_list$counties <- list(split(no_county_data[ , -1], no_county_data$County)) 
json <- toJSON(json_list, prety=TRUE)

file_con <- file('data/covid-19-ma-county.json')
writeLines(json, file_con)
close(file_con)

# Analyze statewide

statewide_raw <- read_csv('data/statewide_data.csv', col_types =
  cols(
    date = col_date("%m/%d/%Y"),
    hospitalizations = col_double(),
    community_transmission = col_double(),
    tests_conducted = col_double(),
    tests_positive = col_double()
  )
)

statewide_analysis <- statewide_raw %>%
  mutate(percent_positive = tests_positive / tests_conducted) %>%
  gather(measure, count, -date) %>%
  arrange(measure, date) %>%
  mutate(dod_change = count - lag(count))

View(statewide_analysis)




