rm(list=ls())
library(here)
# library(data.table)
# library(ggplot2)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general", "visualisation")
source(paste0(script_loc, "libraries_and_file_locs.R"))

# functions ########################
####################################

fns_loc <- paste0(here::here(), "/scripts/R/fns/")
source(paste0(fns_loc, "fuel_moisture_content_fns_trim.R"))

# equilbrium moisture content #####
###################################

# dummy data
# constant temp
df1_const_temp <- data.frame(
  "RH" = rep(c(1:100)/100,4),
  "TEMP_c" = c(rep(0,100), rep(10,100), rep(20,100), rep(30,100)),
  "constant temperature" = c(rep("0 degrees",100), rep("10 degrees",100), rep("20 degrees",100), rep("30 degrees",100))
)

# constant RH
df2_const_RH <- data.frame(
  "RH" = c(rep(0.05, 35), rep(0.20, 35), rep(0.50, 35), rep(0.80, 35)),
  "TEMP_c" = 1:35,
  "relative humidity" = c(rep("5%",35), rep("20%",35), rep("50%",35), rep("80%",35))
)

# to fahrenheit
df1_const_temp$TEMP_f <- celsius_to_fahrenheit(df1_const_temp$TEMP_c)
df2_const_RH$TEMP_f <- celsius_to_fahrenheit(df2_const_RH$TEMP_c)

# get EMC
df1_const_temp$EMC <- apply(df1_const_temp[, c("RH", "TEMP_f")], 1, function(x) get_EMC(x[1], x[2]))
df2_const_RH$EMC <- apply(df2_const_RH[, c("RH", "TEMP_f")], 1, function(x) get_EMC(x[1], x[2]))

# plot
ggplot(data=df1_const_temp, aes(x=RH, y=EMC, group=constant.temperature)) +
  geom_line()

ggplot(data=df2_const_RH, aes(x=TEMP_c, y=EMC, group=relative.humidity)) +
  geom_line()



