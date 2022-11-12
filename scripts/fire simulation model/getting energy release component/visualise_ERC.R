rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = "general"
source(paste0(script_loc, "libraries_and_file_locs.R"))

# functions ########################
####################################

fns_loc <- paste0(here::here(), "/scripts/R/fns/")
source(paste0(fns_loc, "calculating_ERC_fns.R"))

##### GET INPUT DATA ####################
#########################################
this_input_file_loc <- paste0(input_file_loc, "/wildfire simulation model")
this_output_file_loc <- paste0(output_file_loc, "/wildfire simulation model")

this_station_data_w_ERC <- fread(paste0(this_input_file_loc, "/station data/station_data_kettleman_hills_00_22_w_ERC.csv"))

### VISUALISE ERC AND MC ###############
########################################

# ave ERC for each day of year
this_station_data_daily_ave_ERC <- this_station_data_w_ERC %>% 
  group_by(day_of_year) %>%
  summarise(
    average_ERC = mean(ERC),
    month = unique(month)
  )

# ERC by DOY
ggplot(this_station_data_w_ERC, aes(x=day_of_year, y = ERC)) + 
  geom_point()

# ERC for each day in study
ggplot(this_station_data_w_ERC, aes(x=day_of_run, y = ERC, color=as.factor(year))) + 
  geom_point()

# ERC averaged for a year
ggplot(this_station_data_daily_ave_ERC, aes(x=day_of_year, y = average_ERC, color=as.factor(month))) + 
  geom_point()

