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
# install.packages("tseries)
# install.packages("Metrics")
# install.packages("ggplot2")
# install.packages("RSQLite")
# install.packages("geosphere")
# install.packages("leaflet")
# install.packages("cowplot")
# install.packages("ggthemes")


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
  library(RSQLite)

}

if("spatial" %in% req_packages){
  
  message("loading spatial packages")
  library(rgdal)
  library(sf)
  library(raster)
  library(rgeos)
  library(maptools)
  library(geosphere)
  
}

if("statistical" %in% req_packages){
  
  message("loading statistical packages")
  library(tseries)
  library(forecast)
  library(Metrics)
}

if("visualisation" %in% req_packages){
  
  message("loading visualisation packages")
  library(ggplot2)
  library(leaflet)
  library(RColorBrewer)
  library(cowplot)  # for plot_grid fxn
  library(ggthemes)  # install this package for additional ggplot themes
  
}


### FILE LOCS #################
##############################

message("getting file locs")
input_file_loc <- paste0(here::here(), "/data/input") 
output_file_loc <- paste0(here::here(), "/data/output") 





