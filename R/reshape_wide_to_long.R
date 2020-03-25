library(tidyr)
library(readr)
library(dplyr)
library(jsonlite)
library(purrr)
library(glue)
library(readxl)
library(httr)
library(ggplot2)

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


# Country-Level Data ------------------------------------------------------


# Download country-level data from European Centre for Disease Prevention and Control


# create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

# download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

# read the Dataset sheet into “R”

ecdpc_data <- read_excel(tf)

target_countries <- c("Italy", "South_Korea")

select_countries <- ecdpc_data %>%
  filter(`Countries and territories` %in% target_countries) %>%
  select(
    date = DateRep,
    cases_new = Cases,
    deaths_new = Deaths,
    location = `Countries and territories`
  ) %>% 
  arrange(
    location, date
  ) %>%
  group_by(location) %>%
  mutate(
    date = as.Date(date),
    cases_total = cumsum(cases_new),
    cases_percent_change = cases_new / lag(cases_total),
    deaths_total = cumsum(deaths_new),
    deaths_percent_change = deaths_new / lag(deaths_total)
  )
  
ma_total_data <- long %>% 
  ungroup() %>%
  filter(County == "Massachusetts Total") %>%
  mutate(
    date = as.Date(date),
    location = "Massachusetts"
  ) %>%
  select(
    date,
    location,
    cases_total = total_cases,
    cases_new = new_cases,
    cases_percent_change = percent_growth
  )

ma_death_data <- statewide_analysis %>%
  select(
    date,
    deaths_total,
    deaths_new,
    deaths_percent_change
  )

ma_comp_data <- ma_total_data %>%
  left_join(ma_death_data) %>%
  mutate(
    deaths_total = coalesce(deaths_total, 0),
    deaths_new = coalesce(deaths_new, 0),
    deaths_percent_change = coalesce(deaths_percent_change, 0)
  )

italy_data <- select_countries %>% 
  filter(location == "Italy") %>%
  ungroup()

italy_two_weeks_ago <- italy_data %>%
  mutate(
    date = date + 14,
    location = "Italy Two Weeks Ago"
  )

italy_three_weeks_ago <- italy_data %>%
  mutate(
    date = date + 21,
    location = "Italy Three Weeks Ago"
  )


country_data <- bind_rows(
  select_countries, 
  ma_comp_data,
  italy_two_weeks_ago,
  italy_three_weeks_ago
  ) %>%
  mutate(
    cases_2_day_avg_new = (cases_new + lag(cases_new)) / 2
  )


# ggplot(
#   country_data %>% filter(
#   location %in% c("Massachusetts", "Italy Two Weeks Ago"),
#   date >= as.Date("2020-03-01"),
#   date <= Sys.Date()
#   )
#   ) +
#   geom_line(aes(x=date, y=cases_new, colour=location))


ggplot(
  country_data %>% filter(
    location %in% c("Massachusetts", "Italy Three Weeks Ago", "Italy Two Weeks Ago"),
    date >= as.Date("2020-03-01"),
    date <= Sys.Date()
  )
) +
  geom_line(aes(x=date, y=deaths_new, colour=location))

country_json <- country_data %>% toJSON(pretty=TRUE)


# Create JSON -------------------------------------------------------------

# County-level data

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
county_txt <- paste0('{"counties": [', json_str, "]," )

# Country-Comparison
split_list = split(country_data[ , -4], country_data$location)

json_list <- list()
i = 1
for (item in split_list) {
  i_list <- list()
  i_list$location <- names(split_list)[i]
  i_list$data <- item
  json_list[i] <-toJSON(i_list, pretty=TRUE)
  i <- i +1
}

json_str <- glue_collapse(json_list, sep=",")
country_comp_txt <- paste0('\n"country-comparison": [', json_str, "]}" )

full_text <- paste0(county_txt, country_comp_txt)
file_con <- file('data/covid-19-ma-county.json')
writeLines(full_text, file_con)
close(file_con)
