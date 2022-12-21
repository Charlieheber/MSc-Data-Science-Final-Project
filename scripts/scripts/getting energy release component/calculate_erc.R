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

this_fuel_models_vals <- fread(paste0(this_input_file_loc, "/ERC model vals/fuel_model_vals_by_model.csv"))
this_station_data_w_MCs <- fread(paste0(this_input_file_loc, "/fuel moisture contents/station_data_kettleman_hills_00_22_w_fuel_MCs.csv"))

this_fuel_model = "G"

########################################

this_station_data_w_MCs$ERC[1] = NA
for(i in 2:dim(this_station_data_w_MCs)[1]){ # need MC_1000hr after day one so start on day 2
  
  if(this_station_data_w_MCs$year[i-1] != this_station_data_w_MCs$year[i]) message(paste("year", this_station_data_w_MCs$year[i], "\n"))
  
  this_daily_station_data_w_MCs <- this_station_data_w_MCs[i,]
  
  this_station_data_w_MCs$fuel_model <- this_fuel_model
  this_station_data_w_MCs$ERC[i] <- calculate_erc(this_daily_station_data_w_MCs$MC_herb[1], this_daily_station_data_w_MCs$MC_1hr[1], 
                                               this_daily_station_data_w_MCs$MC_10hr[1], this_daily_station_data_w_MCs$MC_100hr[1],
                                               this_daily_station_data_w_MCs$MC_1000hr[1], this_daily_station_data_w_MCs$MC_wood[1], 
                                               this_fuel_model, this_fuel_models_vals)[[1]]
  
}


# visualise result ###################################
######################################################

# ave ERC for each day of year
this_station_data_daily_ave_ERC <- this_station_data_w_MCs %>% 
  group_by(day_of_year) %>%
  summarise(
    average_ERC = mean(ERC),
    month = unique(month)
  )

# ERC for each day in study
ggplot(this_station_data_w_MCs, aes(x=day_of_run, y = ERC, color=as.factor(year))) + 
  geom_point()

# ERC averaged for a year
ggplot(this_station_data_daily_ave_ERC, aes(x=day_of_year, y = average_ERC, color=as.factor(month))) + 
  geom_point()


# save result ########################################
######################################################

write.csv(row.names=FALSE, 
          this_station_data_w_MCs, paste0(this_output_file_loc, "/station_data_kettleman_hills_00_22_w_ERC.csv"))

















