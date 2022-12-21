rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general", "statistical")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET INPUT DATA ####################
#########################################

this_input_file_loc <- paste0(input_file_loc, "/wildfire simulation model")
this_output_file_loc <- paste0(output_file_loc, "/wildfire simulation model")

this_station_dat <- fread(paste0(this_input_file_loc, "/station data/station_data_kettleman_hills_00_22.csv"))

this_wind_df <- data.table(
  "day_of_run" = this_station_dat[!is.na(this_station_dat$mean_wind_direction), "day_of_run"][[1]],
  "ave_wind_speed" = round(this_station_dat[!is.na(this_station_dat$mean_wind_direction), "ave_wind_speed"][[1]]),
  "mean_wind_direction" = this_station_dat[!is.na(this_station_dat$mean_wind_direction), "mean_wind_direction"][[1]],
  "year" = this_station_dat[!is.na(this_station_dat$mean_wind_direction), "year"][[1]]
)

##### VISUALISE ###############################
###############################################

this_wind_df$dir_bins <- findInterval(this_wind_df$mean_wind_direction, seq(0, 360, 5))  

days_by_wind_direction <- this_wind_df %>%
  group_by(dir_bins) %>%
  summarise(
    n_days = n(),
  )
days_by_wind_direction$midpoint <- seq(0, 360, 5)

days_by_wind_speed <- this_wind_df %>%
  group_by(ave_wind_speed) %>%
  summarise(
    n_days = n()
  )


ggplot(data=days_by_wind_direction,aes(x=midpoint,y=n_days))+
  geom_bar(stat="identity")+
  coord_polar()+ xlab("")+ylab("number of days")

ggplot(data=days_by_wind_speed, aes(x=ave_wind_speed, y=n_days))+
  geom_bar(stat="identity")+
  xlab("average daily wind speed (m/s)")+
  ylab("number of days")

##### GET ECDF OF WIND SPEED AND DIRECTION ####
###############################################

ave_wind_speed_ecdf <- ecdf(this_wind_df$ave_wind_speed)
mean_wind_direction_ecdf <-ecdf(this_wind_df$mean_wind_direction)

get_emp_df_val <- function(ecdf, n){
  
  vals = knots(ecdf)
  
  x <- data.frame(
    "vals" = vals,
    "prob" = ecdf(vals) - c(0, ecdf(vals)[1:(length(vals)-1)])
  )
  
  return(sample(x$vals, n, prob=x$prob))
  
}


saveRDS(ave_wind_speed_ecdf, paste0(output_file_loc, "/ecdf_ave_wind_speed.rds"))
saveRDS(mean_wind_direction_ecdf, paste0(output_file_loc, "/ecdf_mean_wind_direction.rds"))

