library('tidyverse')
library('lubridate')
library('wbstats')
library('countrycode')

# Read CSV files ----
positives_world <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
deaths_world <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
recov_world <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))


## Fix world data ----
  
# Extract only the countries we're interested in and change shape 
source('transform_world_v2.R')
# Choose countries to work with
index_countries <- c('South Africa', 'Korea, South', 'US', 'Italy')
# Take original CSV files and perform various calculations, and selects
world_positives_index <- transform_world(positives_world, index_countries, 'positives')
world_recov_index <- transform_world(recov_world, index_countries, 'recovered')
world_deaths_index <- transform_world(deaths_world, index_countries, 'deaths')

## Combine world data and ZA data ----

global_data <- bind_rows(world_positives_index, world_recov_index, world_deaths_index) %>% 
  # Convert province, country, country_code, status to factors
  mutate_if(is.character, as.factor)

## Normalise for population size ----

# Pop size from World Bank Data through wbstats package - Seems like 2018 is latest available data
pop_data <- wb(country = levels(global_data$country_code),
               indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>% 
  select(value, iso3c) %>% 
  rename(country_code = iso3c,
         pop_size = value) %>% 
  mutate_if(is.character, as.factor)

# Normalised cumulative and daily data per 1'000'000 of population
# Specify normalisation factor - some prefer per 100'000 people, others 1'000'000
norm_factor <- 1000000

global_data_norm <- global_data %>%
  # Add population numbers to dataframe for calculation purposes - there must be a better way to do this?
  left_join(pop_data) %>% 
  # Calculated normalised values for cumulative and daily cases
  mutate(norm_cum = cum_cases/pop_size*norm_factor,
         norm_daily = daily_new/pop_size*norm_factor)
 

## Calculate active cases ----

global_data_norm_active <- global_data_norm %>% 
  ## For some reason recovered and deaths are either 0 or NA with random filling
  pivot_wider(names_from = 'status', values_from=c('cum_cases', 'norm_cum'), values_fill=list(c(cum_cases = 0, norm_cum =0))) #%>% 
  # mutate(active_cum_cases = cum_cases_positives - cum_cases_recovered - cum_cases_deaths,
  #        active_norm_cum = norm_cum_positives - cum_cases_recovered - norm_cum_deaths)
  # pivot_longer(cols = c(starts_with('cum_cases'), starts_with)
  
    
    


## Calculate days since 100th case stats ----

# Find the date where each country's positive cases exceeded 100 where the previous day was below 100
# Data may still have provinces in so first have to sum for country over provinces e.g. Australia
global_100th_positive <- global_data %>%
  group_by(country_code, status, date) %>% 
  summarise(cum_country_cases = sum(cum_cases)) %>% 
  select(country_code, date, cum_country_cases, status) %>% 
  filter(cum_country_cases >= 100 & lag(cum_country_cases) < 100 & status == 'positives') %>% 
  rename(hundred_cases_date = date, hundred_status = status)

global_data_100_cases <- global_data_norm %>% 
  # Add date when country reached 100 cases 
  left_join(global_100th_positive) %>% 
  # Filter data out where date before date when country reached 100 cases
  filter(date >= hundred_cases_date) %>% 
  # Calculate number of days since 100th case
  mutate(days_since_100 = as.numeric(difftime(date, hundred_cases_date, units = 'days'))) %>% 
  # Select only useful columns
  select(province, country, lat, long, country_code, date, cum_cases, daily_new, status, norm_cum, norm_daily, days_since_100)
  
## Create plots ----

# Plot cumulative number of cases normalised per 1'000'000 population 

global_data_100_cases %>% 
  # Some countries are split up per province/state and must be added together to get a value for the country
  group_by(country, status, date, days_since_100) %>% 
  summarise(cum_country = sum(cum_cases), 
            daily_new_country = sum(daily_new),
            cum_country_norm = sum(norm_cum),
            daily_new_country_norm = sum(norm_daily)) %>% 
  # Display positives first then recovered, then deaths
  mutate(ordered_status = fct_relevel(status, 'positives', 'recovered', 'deaths')) %>% 
  ggplot(aes(x = days_since_100, y = cum_country_norm, colour = country)) +
  geom_smooth(se = FALSE) +
  facet_grid(vars(ordered_status)) +
  scale_y_log10() +
  xlab('Days since 100th case were reached') +
  ylab('Normalised number of cases (per 1 million)')

# Plot daily new cases vs cumulative cases as per https://aatishb.com/covidtrends/
# Not working

global_data_norm %>% 
  group_by(country, status, date) %>% 
  summarise(cum_country = sum(cum_cases), 
            daily_new_country = sum(daily_new),
            cum_country_norm = sum(norm_cum),
            daily_new_country_norm = sum(norm_daily)) %>% 
  mutate(ordered_status = fct_relevel(status, 'positives', 'recovered', 'deaths')) %>% 
  ggplot(aes(x = cum_country_norm, y = daily_new_country_norm, colour = country)) +
  geom_smooth(se = FALSE) +
  facet_grid(vars(ordered_status)) +
  scale_y_log10() +
  scale_x_log10() +
  xlab('Normalised total number of cases (per 1 million)') +
  ylab('Normalised daily new cases (per 1 million)')
