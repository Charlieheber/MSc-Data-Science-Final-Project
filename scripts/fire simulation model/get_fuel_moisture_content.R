rm(list=ls())
library(tidyverse)
library(data.table)

# functions ########################
####################################

fns_loc <- paste0(here::here(), "/scripts/R/fire simulation model/fns/")
source(paste0(fns_loc, "fuel_moisture_content_fns_trim.R"))

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
verbose <- FALSE
useful_cols <- c("id", "date", "year", "day_of_year", "day_of_run", "total_solar_radiation",	
                 "ave_air_temp", "max_air_temp", "min_air_temp",
                 "ave_relative_humidity", "max_relative_humidity", "min_relative_humidity",
                 "total_precipitation", "latitude", "longitude", "CLIMAT")
measured_vars <- c("total_solar_radiation", "ave_air_temp", "max_air_temp", "min_air_temp",         
                   "ave_relative_humidity", "max_relative_humidity", "min_relative_humidity", "total_precipitation")


file_loc <- paste0(here::here(),  "/data/input/wildfire simulation model/station data/")
this_station_data_all_cols <- fread(paste0(file_loc, "station_data_kettleman_hills_00_22.csv"))

this_station_data <- this_station_data_all_cols[,..useful_cols]

# convert all integer to double
this_station_data <- this_station_data %>% mutate_at(measured_vars, as.numeric)

sapply(this_station_data, class)

# useful quantities ###############
###################################

mm_to_inches <- 0.0393701

month_df <- data.frame("month" = month.name, "month_no" = 1:12)

# useful to add a month/day of month column 
mnth_day_yr <- str_split(this_station_data$date, "/", simplify = TRUE)

this_station_data$month <- mnth_day_yr[,1]   
this_station_data$day_of_month <- mnth_day_yr[,2]

# look at variation by month #######################
# (also get monthly averages for missing values) ###

this_station_data_by_month <- split(this_station_data, this_station_data$month)

this_station_data_month_ave_lst <-  lapply(this_station_data_by_month, 
                                      function(x) data.frame("value" = colMeans(x[, ..measured_vars], na.rm=TRUE)))

for(i in 1:length(this_station_data_month_ave_lst)) { 
  this_station_data_month_ave_lst[[i]]$month <- month_df$month[i]
  this_station_data_month_ave_lst[[i]]$var <- row.names(this_station_data_month_ave_lst[[i]])
}

this_station_data_month_ave_df <- rbindlist(this_station_data_month_ave_lst)

ggplot(this_station_data_month_ave_df[this_station_data_month_ave_df$var %in% c("ave_air_temp", "ave_relative_humidity", "total_precipitation", "total_solar_radiation"),], 
       aes(x=factor(month, level=month.name), y=value, group=var)) +
  geom_line(aes(color=var))+
  geom_point(aes(color=var))
# makes sense!

# deal with missing values ########
###################################

# replace missing values with the average value for that month across all years

replace_missing_val_w_mnth_ave <- function(station_data_by_month, station_data_month_ave_df, month_df){
  # station_data_by_month <- this_station_data_by_month[[1]]
  # station_data_month_ave_df <- this_station_data_month_ave_df
  
  this_month <- month_df[which(month_df$month_no == as.numeric(unique(station_data_by_month$month))),]
  station_data_this_month_ave <- station_data_month_ave_df[which(station_data_month_ave_df$month == this_month$month)]
  
  replacement_lst <- as.list(as.double(station_data_this_month_ave$value))
  names(replacement_lst) <- station_data_this_month_ave$var
  
  station_data_by_month <- station_data_by_month %>% replace_na(replacement_lst)
  
  return(station_data_by_month)
}

this_station_data_cleaned_lst <- lapply(this_station_data_by_month, function(x) replace_missing_val_w_mnth_ave(x, this_station_data_month_ave_df, month_df))

this_station_data_cleaned <- rbindlist(this_station_data_cleaned_lst)

# checks 
this_station_data_cleaned[!complete.cases(this_station_data_cleaned),] # good
replaced_dat <- this_station_data_cleaned[which(this_station_data_cleaned$date %in% this_station_data[!complete.cases(this_station_data),"date"][[1]]),] # also looks good (when compared with data)!

this_station_data <- this_station_data_cleaned
data.table::setorder(this_station_data, id)

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

this_station_data$MC_1hr <- apply(this_station_data[, c("ave_relative_humidity", "ave_air_temp", "state_of_weather")], 1,
                                  function(x) get_MC_1hr(x[1]/100, celsius_to_fahrenheit(x[2]), x[3], FALSE))

# looks good
ggplot(this_station_data, aes(x=day_of_year, y = MC_1hr)) + 
  geom_point()


# get 10 hour dead fuel moisture content ##
###########################################

this_station_data$MC_10hr <- apply(this_station_data[, c("ave_relative_humidity", "ave_air_temp", "state_of_weather")], 1,
                                  function(x) get_MC_10hr(x[1]/100, celsius_to_fahrenheit(x[2]), x[3]))

# looks good
ggplot(this_station_data, aes(x=day_of_year, y = MC_10hr)) + 
  geom_point()


# get 100 hour dead fuel moisture content #
###########################################

this_station_data$daylight_hours <- get_daylight_hours(this_station_data$latitude, this_station_data$day_of_year)

# looks good
ggplot(this_station_data[this_station_data$year == 2001,]) + 
  geom_bar(aes(x=day_of_year, y = daylight_hours), stat="identity", width=1)

# get max/min daily moisture content
this_station_data$EMC_min <- apply(this_station_data[, c("min_relative_humidity", "min_air_temp")], 1, function(x) get_EMC(x[1]/100, celsius_to_fahrenheit(x[2])))
this_station_data$EMC_max <- apply(this_station_data[, c("max_relative_humidity", "max_air_temp")], 1, function(x) get_EMC(x[1]/100, celsius_to_fahrenheit(x[2])))

MC_100hr <- c()
for(i in 1:dim(this_station_data)[1]){
  
  if(i > 1 & verbose){
    if(this_station_data$year[i-1] != this_station_data$year[i]) message(paste("year", this_station_data$year[i], "\n"))
  } 
  
  this_daily_station_data <- this_station_data[i,]
  
  if(i == 1){
    this_initialize_YMC_100hrs <- TRUE
    this_YMC_100hrs <- NA
  } else{
    this_initialize_YMC_100hrs <- FALSE
    this_YMC_100hrs <- MC_100hr[i-1]
  } 

  MC_100hr[i] <- get_MC_100hr(this_daily_station_data$daylight_hours, 
                              this_daily_station_data$EMC_min, 
                              this_daily_station_data$EMC_min,
                              this_daily_station_data$total_precipitation*mm_to_inches,
                              this_YMC_100hrs, 
                              this_initialize_YMC_100hrs, 
                              this_daily_station_data$CLIMAT)
  
    
}

this_station_data$MC_100hr <- MC_100hr

# looks good!
ggplot(this_station_data, aes(x=day_of_year, y = MC_100hr)) + 
  geom_point()


# get 1000 hour dead fuel moisture content #
############################################

# estimate rain duration
this_station_data$rain_duration_hours <- apply(this_station_data[,"total_precipitation"], 1, function(x) get_PPT_duration(mm_to_inches*x, 0.25))

# get weighted 24hr ave boundary condition
this_station_data$BNDRY_T <- apply(this_station_data[, c("daylight_hours", "EMC_min", "EMC_max", "rain_duration_hours")], 1, 
                                   function(x) get_BNDRY_T(x[1], x[2], x[3], x[4]))

this_BNDRY_week <- c("1" = NA, "2" = NA, "3" = NA, "4" = NA, "5" = NA, "6" = NA, "7" = NA)
MC_1000hr_lst <- list()
for(i in 1:dim(this_station_data)[1]){
  
  if(i > 1 & verbose){
    if(this_station_data$year[i-1] != this_station_data$year[i]) message(paste("year", this_station_data$year[i], "\n"))
  } 
  
  this_daily_station_data <- this_station_data[i,]
  
  if(i < 7){
    this_initialize_MC_1000hrs <- TRUE
    this_MC_1000hr_week <- NA
    this_BNDRY_week <- rep(NA,7)
  } else{
    this_initialize_MC_1000hrs <- FALSE
    this_BNDRY_week <- this_station_data[(i-6):i, "BNDRY_T"][[1]]
  } 
  
  MC_1000hr_lst[[i]] <- get_MC_1000hr(this_BNDRY_week, this_MC_1000hr_week, this_initialize_MC_1000hrs, this_daily_station_data$CLIMAT)
  MC_1000hr_lst[[i]]$date <- this_daily_station_data$date
  MC_1000hr_lst[[i]]$id <- this_daily_station_data$id
  
  # if((i %% 7) != 0) MC_1000hr_lst[[i]]$MC_1000hr = NA # only retain MC_1000hr on every 7th day
  
  this_MC_1000hr_week <- MC_1000hr_lst[[i]]$MC_1000hr_week
}

this_MC_1000hr <- rbindlist(lapply(MC_1000hr_lst, function(x) data.table("id" = x$id, "date" = x$date, "MC_1000hr" = x$MC_1000hr)))

this_station_data <- left_join(this_station_data, this_MC_1000hr[, c("id", "MC_1000hr")], by = "id")

# looks good!
ggplot(this_station_data, aes(x=day_of_year, y = MC_1000hr)) + 
  geom_point()

# get herbaceous moisture content ##########
############################################
this_pregreen_DOY <- 10 
this_greenup_DOY <- 30 
this_curing_DOY <- 120
this_MC_herb_pregreen <- NA

this_MC_herb_lst <- list(list("MC_herb" = NA, "date" = this_station_data[1,]$date, "id" = this_station_data[1,]$id))
for(i in 2:dim(this_station_data)[1]){ # need MC_1000hr after day one so start on day 2

  if(this_station_data$year[i-1] != this_station_data$year[i]) message(paste("year", this_station_data$year[i], "\n"))
  
  this_yesterday_station_data <- this_station_data[i-1,]
  this_daily_station_data <- this_station_data[i,]
  
  # get MC herb pregreen on day before greenup stage
  if(this_daily_station_data$day_of_year == this_greenup_DOY){
    this_MC_herb_pregreen = this_MC_herb_lst[[i-1]]$MC_herb
    
    if(this_MC_herb_pregreen > 30) this_MC_herb_pregreen = 30
  }
  
  x <- list(
       "MC_herb" = get_MC_herb(MC_1hr = this_daily_station_data$MC_1hr, 
                               MC_1000hr = this_daily_station_data$MC_1000hr, 
                               MC_1000hr_previous_day = this_yesterday_station_data$MC_1000hr,
                               MC_herb_pregreen=this_MC_herb_pregreen,
                               pregreen_DOY = this_pregreen_DOY,
                               greenup_DOY = this_greenup_DOY,
                               curing_DOY = this_curing_DOY,
                               DOY = this_daily_station_data$day_of_year,
                               CLIMAT = this_daily_station_data$CLIMAT,
                               TEMP_min = celsius_to_fahrenheit(this_daily_station_data$min_air_temp),
                               TEMP_max = celsius_to_fahrenheit(this_daily_station_data$max_air_temp),
                               annuals=FALSE),
       "date" = this_daily_station_data$date,
       "id" = this_daily_station_data$id
       )

  this_MC_herb_lst[[i]] <- x
  
}

head(this_MC_herb_lst)  
this_MC_herb <- rbindlist(lapply(this_MC_herb_lst, function(x) data.table("id" = x$id, "date" = x$date, "MC_herb" = x$MC_herb)))

this_station_data <- left_join(this_station_data, this_MC_herb[, c("id", "MC_herb")], by = "id")

# looks okay? takes time to stabilise - should look into starting value
# weekly oscillation - doesn't make sense
ggplot(this_station_data, aes(x=day_of_year, y = MC_herb, color=as.factor(year))) + 
  geom_point()

# get live wood moisture content #########
##########################################

this_MC_wood_pregreen <- get_MC_wood_pregreen(unique(this_station_data$CLIMAT))

this_MC_wood_lst <- list()
for(i in 2:dim(this_station_data)[1]){ # need MC_1000hr after day one so start on day 2
  
  if(this_station_data$year[i-1] != this_station_data$year[i]) message(paste("year", this_station_data$year[i], "\n"))
  
  this_daily_station_data <- this_station_data[i,]
  
  x <- list(
      "MC_wood" = get_MC_wood(DOY = this_daily_station_data$day_of_year, 
                              pregreen_DOY = this_pregreen_DOY, 
                              greenup_DOY = this_greenup_DOY,
                              MC_1000hr = this_daily_station_data$MC_1000hr,
                              CLIMAT = this_daily_station_data$CLIMAT,
                              MC_wood_pregreen = this_MC_wood_pregreen,
                              MC_wood_previous_day = this_yesterdays_MC_wood$MC_wood),
      "date" = this_daily_station_data$date,
      "id" = this_daily_station_data$id
  )
  
  this_MC_wood_lst[[i]] <- x
  this_yesterdays_MC_wood <- x
}

head(this_MC_wood_lst)  
this_MC_wood <- rbindlist(lapply(this_MC_wood_lst, function(x) data.table("id" = x$id, "date" = x$date, "MC_wood" = x$MC_wood)))

this_station_data <- left_join(this_station_data, this_MC_wood[, c("id", "MC_wood")], by = "id")

ggplot(this_station_data, aes(x=day_of_year, y = MC_wood, color=as.factor(year))) + 
  geom_point()

# save results ###########################
##########################################

write.csv(this_station_data, file = paste0(here::here(), "/data/output/wildfire simulation model/station_data_kettleman_hills_00_22_w_fuel_MCs.csv"))




