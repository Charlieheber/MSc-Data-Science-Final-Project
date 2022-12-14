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
# install.packages("corrplot")
# install.packages("caret")
# install.packages("sp")
# install.packages("mapproj")
# install.packages("ggridges")
# install.packages("neuralnet")
# install.packages("remotes")
# install.packages("devtools")
# remotes::install_github("cran/DMwR")
# install.packages("MASS")
# install.packages("EnvStats")
# install.packages("poweRlaw")
# remotes::install_github("cran/Rothermel")
# install.packages("firebehavioR")
# install.packages("naivebayes")
# install.packages("igraph")

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
  library(sp)
  library(raster)
  library(rgeos)
  library(maptools)
  library(geosphere)
  library(mapproj)
}

if("statistical" %in% req_packages){
  
  message("loading statistical packages")
  library(tseries)
  library(forecast)
  library(Metrics)
  library(caret)
  library(neuralnet)
  library(MASS)
  library(poweRlaw)
  library(EnvStats)
  library(naivebayes)
  library(igraph)
  
  
}

if("visualisation" %in% req_packages){
  
  message("loading visualisation packages")
  theme_set(theme_bw(16))
  library(ggplot2)
  library(leaflet)
  library(RColorBrewer)
  library(cowplot)  # for plot_grid fxn
  library(ggthemes)  # install this package for additional ggplot themes
  library(corrplot)
  library(ggridges)
  library(scales)
  
}

if("fire modelling" %in% req_packages){
  
  message("loading fire modelling packages")
  library(Rothermel)
  library(firebehavioR)
  library(ftsa)
}


### FILE LOCS #################
##############################

message("getting file locs")
input_file_loc <- paste0(here::here(), "/data/input") 
output_file_loc <- paste0(here::here(), "/data/output") 

#### GET CITATIONS ##########
#############################

# citation()
# citation(package = "Rothermel")
# citation(package = "firebehavioR")
# citation(package = "forecast")
# citation(package = "stats")
# citation(package = "naivebayes")



