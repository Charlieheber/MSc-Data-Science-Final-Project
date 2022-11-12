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

this_station_data_w_ERC <- fread(paste0(this_input_file_loc, "/fuel moisture contents/station_data_kettleman_hills_00_22_w_ERC.csv"))

#### TEST DATA FOR STATIONARITY ########
########################################

this_ERC_df <- data.table(
  "day_of_run" = this_station_data_w_ERC[!is.na(this_station_data_w_ERC$ERC), "day_of_run"][[1]],
  "ERC" = this_station_data_w_ERC[!is.na(this_station_data_w_ERC$ERC), "ERC"][[1]]
)

# Augmented Dickey-Fuller (ADF) t-statistic
adf.test(this_ERC_df$ERC)
# need to look into

this_ERC_df$ERC_normalised <- this_ERC_df$ERC/max(this_ERC_df$ERC)

# https://rpubs.com/richkt/269797
plot.new()
frame()
par(mfcol=c(1,2))

# the stationary signal and ACF
plot(this_ERC_df$day_of_run, this_ERC_df$ERC_normalised,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Stationary signal")

acf(this_ERC_df$ERC_normalised, lag.max = length(this_ERC_df$ERC_normalised),
    xlab = "lag #", ylab = 'ACF',main=' ')

#### FIRST DIFF #########################
#########################################

this_number_of_LAGs <- 1

# get ACF of first diff
this_ERC_df$ERC_first_diff <- c(rep(NA, this_number_of_LAGs), diff(this_ERC_df$ERC_normalised, lag=this_number_of_LAGs, differences = 1))

# https://rpubs.com/richkt/269797
plot.new()
frame()
par(mfcol=c(1,2))

# the stationary signal and ACF
plot(this_ERC_df$day_of_run, this_ERC_df$ERC_first_diff,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Stationary signal")

acf(this_ERC_df$ERC_first_diff, lag.max = length(this_ERC_df$ERC_first_diff),
    xlab = "lag #", ylab = 'ACF',main=' ', na.action = na.pass)


# data is stationery 

this_d <- 1

#### Finding P and Q #######################
############################################

# https://rpubs.com/richkt/269797
plot.new()
frame()
par(mfcol=c(1,2))

acf(this_ERC_df$ERC_first_diff, lag.max = length(this_ERC_df$ERC_first_diff),
    xlab = "lag #", ylab = 'ACF',main=' ', na.action = na.pass)

pacf(this_ERC_df$ERC_first_diff, lag.max = length(this_ERC_df$ERC_first_diff),
     xlab = "lag #", ylab = 'ACF',main=' ', na.action = na.pass)


tsData <- ts(data=this_ERC_df$ERC, start=c(2000, 246), frequency=365.25)
plot.new()
frame()
plot(tsData)

components_tsData <- decompose(tsData)
plot(components_tsData)




