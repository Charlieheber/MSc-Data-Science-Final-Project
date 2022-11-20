rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET INPUT DATA ####################
#########################################

input_file_loc <- paste0(input_file_loc, "/wildfire simulation model/")

# station data
this_station_dat_w_ERC <- fread(paste0(input_file_loc, "/station data/station_data_kettleman_hills_00_22_w_ERC.csv"))
this_station_dat_w_wind_speed <- fread(paste0(input_file_loc, "/station data/station_data_kettleman_hills_00_22.csv"))

# historical fire data
this_fire_dat <- fread(paste0(input_file_loc, "/historical fire data/kettleman_hils_2000_2015_100_km_ignition_days_by_fire_size_class.csv"))


#### JOIN RELEVANT DATA #################
#########################################

colnames(this_station_dat_w_ERC)

this_fire_dat_w_ERC_wind_speed_raw <- this_station_dat_w_ERC[, c("id", "date", "year", "day_of_year", "ERC")] %>%
  left_join(this_station_dat_w_wind_speed[, c("id","ave_wind_speed", "mean_wind_direction", "max_wind_gust")], by="id") %>%
  left_join(this_fire_dat[, c("DISCOVERY_DOY", "id", "FIRE_SIZE_CLASS_A", "FIRE_SIZE_CLASS_B", "FIRE_SIZE_CLASS_C", "FIRE_SIZE_CLASS_D", "FIRE_SIZE_CLASS_E",
                              "FIRE_SIZE_CLASS_F", "FIRE_SIZE_CLASS_G")], by="id")


# only keep days we have fires for (so not after 2015)
this_fire_dat_w_ERC_wind_speed <- this_fire_dat_w_ERC_wind_speed_raw[this_fire_dat_w_ERC_wind_speed_raw$year <= 2015,]
this_fire_dat_w_ERC_wind_speed <- this_fire_dat_w_ERC_wind_speed[!is.na(this_fire_dat_w_ERC_wind_speed$ERC),]

# replace nas w/ 0s
this_fire_dat_w_ERC_wind_speed <- this_fire_dat_w_ERC_wind_speed %>% 
  replace_na(list(FIRE_SIZE_CLASS_A=0, FIRE_SIZE_CLASS_B=0, FIRE_SIZE_CLASS_C=0,
                  FIRE_SIZE_CLASS_D=0, FIRE_SIZE_CLASS_E=0, FIRE_SIZE_CLASS_F=0,
                  FIRE_SIZE_CLASS_G=0))


this_fire_dat_w_ERC_wind_speed[which(this_fire_dat_w_ERC_wind_speed$ERC > 100), "ERC"] <- 100



#### GET USEFUL VARS ###################
########################################

# A=0-0.25 acres, 
# B=0.26-9.9 acres, 
# C=10.0-99.9 acres, 
# D=100-299 acres, 
# E=300 to 999 acres, 
# F=1000 to 4999 acres, 
# and G=5000+ acres

this_fire_dat_w_ERC_wind_speed$num_ignitions <- (# this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_A +
  this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_B +
    this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_C +
    this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_D +
    this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_E +
    this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_F +
    this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_G)

this_fire_dat_w_ERC_wind_speed$num_large_fire_ignitions <- (this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_D +
                                                              this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_E +
                                                              this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_F +
                                                              this_fire_dat_w_ERC_wind_speed$FIRE_SIZE_CLASS_G)


this_fire_dat_w_ERC_wind_speed <- this_fire_dat_w_ERC_wind_speed %>%
  mutate(
    fire_ignition_day = case_when(
      num_ignitions > 0 ~ TRUE,
      num_ignitions == 0 ~ FALSE
    ),
    large_fire_ignition_day = case_when(
      num_large_fire_ignitions > 0 ~ TRUE,
      num_large_fire_ignitions == 0 ~ FALSE
    ),
    multi_fire_ignition_day = case_when(
      num_ignitions >= 1 ~ TRUE,
      num_ignitions < 1 ~ FALSE
    )
  )

#### SAVE RESULT ###########################
############################################

write.csv(this_fire_dat_w_ERC_wind_speed, 
          paste0(output_file_loc, "/wildfire simulation model/station_data_kettleman_hills_00_22_w_ERC_fire_ignitions.csv"),
          row.names = FALSE)










