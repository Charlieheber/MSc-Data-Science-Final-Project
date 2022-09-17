rm(list=ls())
library(tidyverse)
library(data.table)

# functions ########################
####################################

fns_loc <- paste0(here::here(), "/scripts/R/fire simulation model/fns/")
source(paste0(fns_loc, "fuel_moisture_content_fns.R"))

get_year_max_solar_radiation <- function(station_data){ 
  
  station_data_lst_by_year <- split(station_data, station_data$year)
  
  max_solar_rad_lst <- lapply(station_data_lst_by_year, function(x) data.frame("max_year_solar_radiation" = max(x$total_solar_radiation)))
  
  max_solar_rad_df <- data.table(
    "year" = as.numeric(names(max_solar_rad_lst)),
    rbindlist(max_solar_rad_lst)
  )
  
  return(max_solar_rad_df)
  
} # to find yearly maximum solar radiation

# load station data #################
#####################################
useful_cols <- c("date", "year", "day_of_year", "day_of_run", "total_solar_radiation",	
                 "ave_air_temp", "max_air_temp", "min_air_temp",
                 "ave_relative_humidity", "max_relative_humidity", "min_relative_humidity",
                 "total_precipitation", "latitude", "longitude", "CLIMAT")


file_loc <- paste0(here::here(),  "/data/input/wildfire simulation model/station data/")
this_station_data_all_cols <- fread(paste0(file_loc, "station_data_kettleman_hills_00_22.csv"))

this_station_data <- this_station_data_all_cols[,..useful_cols]

# useful quantities ###############
###################################

mm_to_inches <- 0.0393701
measured_vars <- c("total_solar_radiation", "ave_air_temp", "max_air_temp", "min_air_temp",         
                   "ave_relative_humidity", "max_relative_humidity", "min_relative_humidity", "total_precipitation")

# useful to add a month/day of month column 
mnth_day_yr <- str_split(this_station_data$date, "/", simplify = TRUE)

this_station_data$month <- mnth_day_yr[,1]   
this_station_data$day_of_month <- mnth_day_yr[,2]

# look at variation by month #######################
# (also get monthly averages for missing values) ###

this_station_data_by_month <- split(this_station_data, this_station_data$month)

station_data_month_ave_lst <-  lapply(this_station_data_by_month, 
                                      function(x) data.frame("value" = colMeans(x[, ..measured_vars], na.rm=TRUE)))

for(i in 1:length(station_data_month_ave_lst)) { 
  station_data_month_ave_lst[[i]]$month <- month.name[i]
  station_data_month_ave_lst[[i]]$var <- row.names(station_data_month_ave_lst[[i]])
}

station_data_month_ave_df <- rbindlist(station_data_month_ave_lst)

ggplot(station_data_month_ave_df[station_data_month_ave_df$var %in% c("ave_air_temp", "ave_relative_humidity", "total_precipitation", "total_solar_radiation"),], 
       aes(x=factor(month, level=month.name), y=value, group=var)) +
  geom_line(aes(color=var))+
  geom_point(aes(color=var))


# deal with missing values ########
###################################


names(this_station_data)

this_station_data[!complete.cases(this_station_data), ]

# get state of weather code ########
####################################

state_of_weather_key <- data.table(
  "state_of_weather" = c(0:6),
  "weather" = c("clear", "scattered clouds", "broken clouds", "overcast", "fog", "drizzle", "rain")
)

head(this_station_data)

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

ggplot(state_of_weather_count) + geom_bar(aes(fill = weather, x="", y = n), stat="identity", width=1) +
  coord_polar("y", start=0) +  theme_void()

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

# looks good
ggplot(this_station_data, aes(x=day_of_year, y = MC_10hr)) + 
  geom_point()

# get 100 hour dead fuel moisture content #
###########################################

this_station_data$daylight_hours <- get_daylight_hours(this_station_data$latitude, this_station_data$day_of_year)

# looks good
ggplot(this_station_data[this_station_data$year == 2001,]) + 
  geom_bar(aes(x=day_of_year, y = daylight_hours), stat="identity", width=1)

MC_100hr <- c()
for(i in 1:dim(this_station_data)[1]){
  
  if(i > 1){
    if(this_station_data$year[i-1] != this_station_data$year[i]) message(paste("year", this_station_data$year[i], "\n"))
  } 
  
  this_daily_station_data <- this_station_data[i,]
  
  this_EMC_max <- get_EMC(this_daily_station_data$max_relative_humidity/100, celsius_to_fahrenheit(this_daily_station_data$max_air_temp))
  this_EMC_min <- get_EMC(this_daily_station_data$min_relative_humidity/100, celsius_to_fahrenheit(this_daily_station_data$min_air_temp))
  
  if(i == 1){
    this_initialize_YMC_100hrs <- TRUE
    this_YMC_100hrs <- NA
  } else{
    this_initialize_YMC_100hrs <- FALSE
    this_YMC_100hrs <- MC_100hr[i-1]
  } 

  MC_100hr[i] <- get_MC_100hr(this_daily_station_data$daylight_hours, 
                              this_EMC_min, this_EMC_max,
                              this_daily_station_data$total_precipitation,
                              this_YMC_100hrs, 
                              this_initialize_YMC_100hrs, 
                              this_daily_station_data$CLIMAT)
  
    
}

this_station_data$MC_100hr <- MC_100hr




















