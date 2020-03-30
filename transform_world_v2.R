transform_world <- function(world_df, countries, status){
  world_df <- world_df %>% 
    # Make column names easier to work with as object names
    rename(country = `Country/Region`,
           province = `Province/State`,
           lat = Lat,
           long = Long) 
  
  # Extract column names to use later
  world_cols <- colnames(world_df)
  
  world_index_df <- world_df %>% 
    # Get only index countries from input vector
    filter(country %in% countries) %>%  
    # Fix country names for South Korea to make it easier to work with (, is a problem)
    mutate(country = case_when(
     country == 'Korea, South' ~ 'South Korea',
     TRUE ~ country)) %>%
    # Add country iso3c codes as column for later use with worldbank data
    mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>% 
    # Change the shape of the data to long but keep Province/State, date, Country/Region, and cumulative cases per date
    pivot_longer(cols = world_cols[5:length(world_cols)], names_to = "date", values_to = 'cum_cases')  %>% 
    # Change dates to date format
    mutate(date = mdy(date)) %>%     
    # Calculate daily new cases per country
    # Implement ifelse to make sure 1st occurrence of new country/province is not negative number but equal to first reported number of cases
    # Sometimes a previous day may be adjusted downwards in the data so a negative number in daily cases is not impossible
    mutate(daily_new = ifelse((lag(cum_cases) > cum_cases) & (lag(country) != country | lag(province) != province) , 
           cum_cases , cum_cases - lag(cum_cases))) %>% 
    # Add status to be able to do analysis based on 'positive', 'recovered', 'dead'
    mutate(status = status) 

  return(world_index_df)
}

# For testing purposes
# my_countries = c('Korea, South', 'South Africa', 'Italy', 'Australia')
# test_df <- transform_world(positives_world, my_countries, 'positive')

