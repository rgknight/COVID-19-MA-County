library(tidyr)
library(readr)
library(dplyr)
library(jsonlite)
library(purrr)
library(glue)

wide <- read_csv('data/covid-19-ma-county-wide.csv')

# Reshape to long format, suitable for Tableau / R
long <- wide %>%
  gather(date, total_cases, 
         -`County`, -`State`, -Lat, -Long) %>%
  mutate(
    date=as.Date(date, "%m/%d/%Y")
  ) %>%
  arrange(County, date) %>%
  group_by(County) %>%
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

counties_list <- split(no_county_data[ , -1], no_county_data$County)

json_list <- list()
i = 1
for (county_data in counties_list) {
  county_i_list <- list()
  county_i_list$county <- names(counties_list)[i]
  county_i_list$data <- county_data
  json_list[i] <-toJSON(county_i_list, pretty=TRUE)
  i <- i +1
}

json_str <- glue_collapse(json_list, sep=",")
txt <- paste0('{"counties": [', json_str, "]}" )
file_con <- file('data/covid-19-ma-county.json')
writeLines(txt, file_con)
close(file_con)

# Analyze statewide

statewide_raw <- read_csv('data/statewide_data.csv', col_types =
  cols(
    date = col_date("%m/%d/%Y"),
    hospitalizations = col_double(),
    community_transmission = col_double(),
    public_lab_tests = col_double(),
    public_lab_positive = col_double(),
    private_lab_positive = col_double(),
    private_lab_tests = col_double()
  )
)

statewide_analysis <- statewide_raw %>%
  mutate(
    tests_conducted = public_lab_tests + private_lab_tests,
    tests_positive = public_lab_positive + private_lab_positive
    ) %>%
  gather(measure, total, -date) %>%
  arrange(measure, date) %>%
  group_by(measure) %>%
  mutate(
    new = total - lag(total),
    percent_change = new / lag(total)
    ) %>%
  gather(measure2, value, total, new, percent_change) %>%
  unite("key", measure, measure2) %>%
  spread(key, value) %>%
  mutate(
    tests_percent_positive_total = tests_positive_total / tests_conducted_total,
    tests_percent_positive_new = tests_positive_new / tests_conducted_new
  )



View(statewide_analysis)




