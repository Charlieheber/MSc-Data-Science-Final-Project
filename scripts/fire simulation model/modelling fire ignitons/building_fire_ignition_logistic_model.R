rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation")
source(paste0(script_loc, "libraries_and_file_locs.R"))

# functions ########################
####################################

# fns_loc <- paste0(here::here(), "/scripts/R/fns/")
# source(paste0(fns_loc, ".R"))

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


#### GET USEFUL VARS ###################
########################################

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


#### VISUALISE ##########################
#########################################
display.brewer.all()

# lets plot ERC against igntion days (0 = no ignitions, 1 = ignitions) and colour by largest fire size ignited that day
fire_color = "lightcoral"
large_fire_color = "lightsteelblue"
multi_fire_color = "teal"

# FIRE IGNITION DAYS
# ERC
fire_ignition_day_ERC_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=ERC, y=fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=fire_color) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")

# max wind gust
fire_ignition_day_max_wind_gust_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=max_wind_gust, y=fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=fire_color) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Fire Ignition Days")


large_fire_ignition_day_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=ERC, y=large_fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=large_fire_color) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Large Fire Ignition Days (> 100 acres)")

multi_fire_ignition_day_plot <- ggplot(this_fire_dat_w_ERC_wind_speed, aes(x=ERC, y=multi_fire_ignition_day)) +
  geom_jitter(width = 0, height = 0.1, color=multi_fire_color) +
  labs(title = "Kettleman Hills 2000-2015: ERC on Mulit Fire Ignition Days")

plot_grid(fire_ignition_day_plot, large_fire_ignition_day_plot, multi_fire_ignition_day_plot)

#### BUILD LOGISTIC MODELS ###############
##########################################
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4828741/
# 1) Probabilty of fire ignition
# 2) Probabilty of large fire given an ignition

# explanatory variables:  ERC(G), wind speed, maximum wind speed

# 1) Probabilty of fire ignition

# UNIVARIABLE ANALYSIS
colnames(this_fire_dat_w_ERC_wind_speed)

x <- runif(length(max_wind_gust))

# ERC <- this_fire_dat_w_ERC_wind_speed[, "ERC"][[1]]
# max_wind_gust <- this_fire_dat_w_ERC_wind_speed[, "max_wind_gust"][[1]]
# ave_wind_speed <- this_fire_dat_w_ERC_wind_speed[, "ave_wind_speed"][[1]]
# mean_wind_direction <- this_fire_dat_w_ERC_wind_speed[, "mean_wind_direction"][[1]]
# 
# fire_ignition <- this_fire_dat_w_ERC_wind_speed[, "fire_ignition_day"][[1]]

univariable.ERC <- glm(fire_ignition_day~ERC, family=binomial(link="logit"), data=this_fire_dat_w_ERC_wind_speed)
univariable.max_wind_gust <- glm(fire_ignition_day~max_wind_gust, family=binomial(link="logit"), data=this_fire_dat_w_ERC_wind_speed)
univariable.ave_wind_speed <- glm(fire_ignition_day~ave_wind_speed, family=binomial, data=this_fire_dat_w_ERC_wind_speed)
univariable.mean_wind_direction <- glm(fire_ignition_day~mean_wind_direction, family=binomial, data=this_fire_dat_w_ERC_wind_speed)

summary(univariable.ERC)
summary(univariable.max_wind_gust)
summary(univariable.ave_wind_speed)
summary(univariable.mean_wind_direction)



