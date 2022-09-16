rm(list=ls())
library(tidyverse)
library(data.table)

# functions ########################
####################################

fns_loc <- paste0(here::here(), "/scripts/R/fire simulation model/fns/")
source(paste0(fns_loc, "fuel_moisture_content_fns.R"))

# load station data #################
#####################################

file_loc <- paste0(here::here(),  "/data/input/wildfire simulation model/station data/")
this_station_data <- fread(paste0(file_loc, "station_data_kettleman_hills_00_22.csv"))

# useful quantities ###############
###################################

mm_to_inches <- 0.0393701

# get state of weather code ########
####################################

state_of_weather_key <- data.table(
  "state_of_weather" = c(0:6),
  "weather" = c("clear", "scattered clouds", "broken clouds", "overcast", "fog", "drizzle", "rain")
)

head(this_station_data)

# need to find yearly maximum solar radiation
get_year_max_solar_radiation <- function(station_data){
  
  station_data_lst_by_year <- split(station_data, station_data$year)
  
  max_solar_rad_lst <- lapply(station_data_lst_by_year, function(x) data.frame("max_year_solar_radiation" = max(x$total_solar_radiation)))
    
  max_solar_rad_df <- data.table(
    "year" = as.numeric(names(max_solar_rad_lst)),
    rbindlist(max_solar_rad_lst)
  )
  
  return(max_solar_rad_df)
  
}

this_year_max_solar_df <- get_year_max_solar_radiation(this_station_data)
this_station_data <- this_station_data %>% left_join(this_year_max_solar_df, by="year")

# get state of weather codes
this_station_data$state_of_weather <- apply(this_station_data[,c("total_solar_radiation", "max_year_solar_radiation", "total_precipitation")],
                                            1,
                                            function(x) get_state_of_weather_code(x[1], x[2], x[3]*mm_to_inches)
                                            )
# check result
state_of_weather_count <- this_station_data %>% count(state_of_weather)
state_of_weather_count <- state_of_weather_count %>% left_join(state_of_weather_key, by="state_of_weather")

ggplot(state_of_weather_count) + geom_bar(aes(x = weather, y = n), stat="identity")

# get 1 hour dead fuel moisture content ###
###########################################

get_MC_1hr(this_station_data$ave_relative_humidity[1]/100, celsius_to_fahrenheit(this_station_data$ave_air_temp[1]),
           this_station_data$state_of_weather[1], FALSE, FALSE)

this_station_data$MC_1hr <- apply(this_station_data[, c("ave_relative_humidity", "ave_air_temp", "state_of_weather")], 1,
                                  function(x) get_MC_1hr(x[1]/100, celsius_to_fahrenheit(x[2]), x[3], FALSE))


# get 10 hour dead fuel moisture content ##
###########################################

get_MC_10hr(this_station_data$ave_relative_humidity[1]/100, celsius_to_fahrenheit(this_station_data$ave_air_temp[1]))

this_station_data$MC_10hr <- apply(this_station_data[, c("ave_relative_humidity", "ave_air_temp")], 1,
                                  function(x) get_MC_10hr(x[1]/100, celsius_to_fahrenheit(x[2])))


# get 100 hour dead fuel moisture content #
###########################################

this_station_data$daylight_hours <- get_daylight_hours(this_station_data$latitude, this_station_data$day_of_year)

this_station_data$MC_10hr <- apply(this_station_data[, c("ave_relative_humidity", "ave_air_temp")], 1,
                                   function(x) get_MC_100hr(x[1]/100, celsius_to_fahrenheit(x[2])))


















