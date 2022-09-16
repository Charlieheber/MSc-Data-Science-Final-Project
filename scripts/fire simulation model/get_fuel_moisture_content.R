rm(list=ls())
library(data.table)


file_loc <- paste0(here::here(),  "/data/input/wildfire simulation model/")


fread(paste0(file_loc, "station_data_ash_meadows_nevada_sept_2016.csv"))
