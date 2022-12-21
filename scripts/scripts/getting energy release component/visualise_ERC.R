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
this_station_data_w_ERC$year <- as.factor(this_station_data_w_ERC$year)

this_fire_dat <- fread(paste0(this_input_file_loc, "/historical fire data/kettleman_hils_2000_2015_100_km_fires.csv"))
this_fire_dat$FIRE_YEAR <- as.factor(this_fire_dat$FIRE_YEAR)
### VISUALISE ERC AND MC ###############
########################################

# ave ERC for each day of year
this_station_data_daily_ave_ERC <- this_station_data_w_ERC %>% 
  group_by(day_of_year) %>%
  summarise(
    average_ERC = mean(ERC),
    month = unique(month)
  )

this_station_data_daily_ave_ERC$month <- as.factor(this_station_data_daily_ave_ERC$month)

# ERC by DOY
ggplot(this_station_data_w_ERC, aes(x=day_of_year, y = ERC)) + 
  geom_point(size=3)+ xlab("day of year") + ylab("ERC") +
  theme(legend.position =  "none",
        text =element_text(size=26))


# ERC for each day in study
ggplot(this_station_data_w_ERC, aes(x=day_of_run, y = ERC, color=year)) + 
  geom_point() + ylab("daily ERC") + xlab("day of study period") +
  theme(legend.position =  "none")


# ERC averaged for a year
ggplot(this_station_data_daily_ave_ERC, aes(x=day_of_year, y = average_ERC, color=month)) + 
  geom_point(size=4) + ylab("average daily ERC") + xlab("day of year") +
  theme(legend.position =  "none",
        text =element_text(size=30))


####VISUALISE ERC w/ FIRE SIZE ########
#######################################

# ave ERC + sum FIRE size by month 
this_station_dat_by_yr_mnth <- this_station_data_w_ERC %>% 
  group_by(year, month) %>%
  summarise(
    average_ERC = mean(ERC, na.rm=TRUE)
  )


this_fire_dat_by_yr_mnth <- this_fire_dat %>%
  group_by(FIRE_YEAR, DISC_MONTH) %>%
  summarise(
    sum_FIRE_SIZE = sum(FIRE_SIZE, na.rm=TRUE),
    n_FIRES = n()
  )

this_station_fire_dat <- this_station_dat_by_yr_mnth %>%  left_join(this_fire_dat_by_yr_mnth, by =c("year" = "FIRE_YEAR", "month" = "DISC_MONTH"))
this_station_fire_dat$year_mnth <- paste0(this_station_fire_dat$year, "-", this_station_fire_dat$month)

# ERC averaged for a year
ggplot(this_station_fire_dat, aes(x=average_ERC, y = n_FIRES)) + 
  geom_point(size=4)+ xlab("average monthly ERC") + ylab("number of ignitions") +
  theme(legend.position =  "none",
        text =element_text(size=34))




