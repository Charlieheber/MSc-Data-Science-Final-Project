# install.packages("tidyverse")
# install.packages("rgdal")
# install.packages("here")
# install.packages("sf")
# install.packages("rgeos")
# install.packages("maptools")
# install.packages("raster")
# install.packages("odbc")
# install.packages("data.table")
# install.packages("DBI")

### LIBRARIES ###############
#############################

if("general" %in% req_packages){
  
  message("loading general packages")
  library(here)
  library(tidyverse)
  library(data.table)
  
}

if("SQL" %in% req_packages){
  
  message("loading SQL packages")
  library(odbc)
  library(DBI)
}

if("spatial" %in% req_packages){
  
  message("loading spatial packages")
  library(rgdal)
  library(sf)
  library(raster)
  library(rgeos)
  library(maptools)
  
}

### FILE LOCS #################
##############################

message("getting file locs")
input_file_loc <- paste0(here::here(), "/data/input") 
output_file_loc <- paste0(here::here(), "/data/output") 





