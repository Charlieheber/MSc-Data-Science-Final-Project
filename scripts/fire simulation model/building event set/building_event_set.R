rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "visualisation", "statistical")
source(paste0(script_loc, "libraries_and_file_locs.R"))

##### GET MODELS ########################
#########################################

file_input_loc <- paste0(input_file_loc, "/wildfire simulation model/models/")

generate_ERC <- read_rds(paste0(file_input_loc, "ERC_TSM_ARIMA.rds")) # TS Analysis - GET ERC 
generate_fire_igntion_days <- read_rds(paste0(file_input_loc, "fire_ignition_day_vars_ERC.rds")) # Logistic Model - Get  Fire Ignition Days
source(paste0(script_loc, "fns/fire_size_ln_model_fns.R")) # Lognormal Fire Size Distribution

#### GET INPUT DATA ####################
########################################

this_agg_fire_sizes <- fread(paste0(input_file_loc, "/wildfire simulation model/fire sizes/agg_fire_sizes_all_years_by_landtype.csv"))

