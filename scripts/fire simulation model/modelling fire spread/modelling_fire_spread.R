rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "spatial", "visualisation")
source(paste0(script_loc, "libraries_and_file_locs.R"))

#### BUILD GRID SHAPEFILE ################
##########################################
this_n_cols <- 8
this_n_rows <- 10
this_lat_extent <- c(-122, -117)
this_lon_extent <- c(34, 38)

create_grid_shp <- function(n_cols, n_rows, lat_extent, lon_extent){
  
  # n_cols = this_n_cols
  # n_rows = this_n_rows
  # lat_extent = this_lat_extent
  # lon_extent = this_lon_extent
  
  # create grid raster
  grid_raster <- raster(extent(matrix(c(lat_extent[1], lon_extent[1], lat_extent[2],  lon_extent[2]), nrow=2)), 
                        nrow=n_rows, ncol=n_cols, 
                        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")            
  grid_raster[] <- 1:ncell(grid_raster)
  
  # turn to shp
  grid_shp <- as(grid_raster, "SpatialPolygonsDataFrame")
  
  # add shp data
  grid_shp_ids <- data.frame("locnum" = 1:ncell(grid_shp))
  grid_shp_lat_lons <- rbindlist(lapply(grid_shp@polygons, function(x) data.frame("lon" = x@labpt[1], "lat" = x@labpt[2])))
  grid_shp_col_rows <- data.frame("colNum" = rep(1:n_cols, times=n_rows), "rowNum" = rep(1:n_rows,each=n_cols))
  
  grid_shp@data <- do.call("cbind", list(grid_shp_ids, grid_shp_lat_lons, grid_shp_col_rows))
  
  return(grid_shp)
  
}

this_grid_shp <- create_grid_shp(this_n_cols, this_n_rows, this_lat_extent, this_lon_extent)

##### VISUALISE ###########################
###########################################
head(this_grid_shp@data)
class(this_grid_shp)   
plot(this_grid_shp) 


leaflet(this_grid_shp) %>%
  addPolygons() %>%
  addLabelOnlyMarkers(data=this_grid_shp@data, 
                      lng=~lon, lat=~lat, 
                      label=~locnum, 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T,
                                                  offset = c(0, 5)))


#### ADD SOME RANDOM BURN DELAYS ########
#### FOR EACH GRID SQUARE ###############

# this_grid_shp$fire_spread_rate <- round(runif(dim(this_grid_shp)[1], 0, 100))
this_grid_shp$fire_spread_rate <- 1
this_grid_shp$grid_length <- 10

head(this_grid_shp@data)
#### BUILD FUNCTION FOR FINDING NEAREST #
#### NEIGHBOURS #########################

find_nearest_neighbours <- function(grid, grid_cell){
  
  # grid_shp = this_grid_shp
  # grid_cell = this_grid_shp[this_grid_shp$locnum == 36,]
  
  grid_NNs <- grid[(grid$rowNum %in% c(grid_cell$rowNum-1, grid_cell$rowNum, grid_cell$rowNum+1) &
                      grid$colNum %in% c(grid_cell$colNum-1, grid_cell$colNum, grid_cell$colNum+1)), ]
  
  grid_NNs <- grid_NNs %>%
    mutate(
      NN_direction = case_when(
        (locnum == grid_cell$locnum) ~ "origin",
        (colNum == grid_cell$colNum & rowNum == (grid_cell$rowNum - 1)) ~ "N",
        (colNum == (grid_cell$colNum + 1) & rowNum == (grid_cell$rowNum - 1)) ~ "NE",
        (colNum == (grid_cell$colNum + 1) & rowNum == grid_cell$rowNum) ~ "E",
        (colNum == (grid_cell$colNum + 1) & rowNum == (grid_cell$rowNum + 1)) ~ "SE",
        (colNum == grid_cell$colNum & rowNum == (grid_cell$rowNum + 1)) ~ "S",
        (colNum == (grid_cell$colNum - 1) & rowNum == (grid_cell$rowNum + 1)) ~ "SW",
        (colNum == (grid_cell$colNum - 1) & rowNum == (grid_cell$rowNum)) ~ "W",
        (colNum == (grid_cell$colNum - 1) & rowNum == (grid_cell$rowNum - 1)) ~ "NW",
      )
    )
  
  return(grid_NNs)
  
} 
get_burn_delay <- function(grid_cell_NNs, current_burn_time){
  
  # grid_cell_NNs <- this_grid_cell_NNs  
  # current_burn_time <- this_current_burn_time
  
  
  grid_lengths <- grid_cell_NNs$grid_length 
  names(grid_lengths) <- grid_cell_NNs$NN_direction
  
  fire_spread_rates <- grid_cell_NNs$fire_spread_rate
  names(fire_spread_rates) <- grid_cell_NNs$NN_direction
  
  get_burn_delay_given_direction <- function(grid_length, fire_spread_rate, NN_grid_length, NN_fire_spread_rate, direction){
    
    diagonal_grid_length <- grid_length*sqrt(2)
    NN_diagonal_grid_length <- NN_grid_length*sqrt(2)
    
    if(direction %in% c("N", "E", "S", "W")){
      burn_delay = (grid_length/fire_spread_rate + NN_grid_length/NN_fire_spread_rate)/2
      
    } else if(direction %in% c("NE", "SE", "SW", "NW")){
      burn_delay = (diagonal_grid_length/fire_spread_rate + NN_diagonal_grid_length/NN_fire_spread_rate)/2
      
    }
    
    return(burn_delay)
    
  }
  
  grid_cell_NNs <- grid_cell_NNs %>%
    mutate(
      burn_delay = case_when(
        NN_direction == "N" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["N"], fire_spread_rates["N"], "N") + current_burn_time, 
        NN_direction == "NE" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["NE"], fire_spread_rates["NE"], "NE") + current_burn_time,
        NN_direction == "E" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["NE"], fire_spread_rates["E"], "E") + current_burn_time,
        NN_direction == "SE" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["SE"], fire_spread_rates["SE"], "SE") + current_burn_time,
        NN_direction == "S" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["S"], fire_spread_rates["S"], "S") + current_burn_time,
        NN_direction == "SW" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["SW"], fire_spread_rates["SW"], "SW") + current_burn_time,
        NN_direction == "W" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["W"], fire_spread_rates["W"], "W") + current_burn_time,
        NN_direction == "NW" ~ get_burn_delay_given_direction(grid_length, fire_spread_rate, grid_lengths["NW"], fire_spread_rates["NW"], "NW") + current_burn_time
      )
    )
  
  return(grid_cell_NNs)
  
}

run_burn_simulation <- function(grid, fire_igntion_cell){
  
  # grid <- this_grid 
  # fire_igntion_cell <- 44
  
  # add res columns
  grid$burn_delay <- 0
  grid$burning <- FALSE
  grid$found_NNs <- FALSE
  
  # add first cell
  grid[grid$locnum == fire_igntion_cell, "burning"] <- TRUE
  head(grid)
  
  # initiate active fire grid (so any unburnt cells)
  active_fire_grid <- grid
  
  # initiate burn time
  current_burn_time <- 0
  
  # WHILE LOOP START HERE
  while(any(!grid$burning)){
    
    Sys.sleep(0.1)
    message(paste("active fire cell:", fire_igntion_cell))
    
    # fire nearest neighbours
    grid_cell_NNs <- find_nearest_neighbours(active_fire_grid, grid[grid$locnum == fire_igntion_cell,])
    head(grid_cell_NNs)
    
    # get burn delay for each NN
    grid_cell_NNs_w_burn_delay <- get_burn_delay(grid_cell_NNs, current_burn_time)
    head(grid_cell_NNs_w_burn_delay)
    
    # add current burn delay to full grid
    grid <- grid %>% left_join(grid_cell_NNs_w_burn_delay[c("locnum", "burn_delay")], by="locnum", suffix = c("", "_current")) %>%
      replace_na(list(burn_delay_current=0))
    # grid <- grid_w_current_burn_delay
    head(grid)
    
    # grid_w_current_burn_delay$burn_delay <- grid_w_current_burn_delay$burn_delay +  grid_w_current_burn_delay$burn_delay_current 
    grid$burn_delay <- grid$burn_delay +  grid$burn_delay_current
    
    # make sure squares are listed as burnt
    # grid_w_current_burn_delay[which(grid_w_current_burn_delay$burn_delay > 0), "burning"] <- TRUE
    grid[which(grid$burn_delay > 0), "burning"] <- TRUE
    
    
    # make sure current square is logged as found NNs
    # grid_w_current_burn_delay[grid_w_current_burn_delay$locnum == fire_igntion_cell, "found_NNs"] <- TRUE
    grid[grid$locnum == fire_igntion_cell, "found_NNs"] <- TRUE
    
    
    grid$burn_delay_current <- NULL
    
    # start with new fire cell   
    # fire_igntion_cell <- grid_cell_NNs_w_burn_delay[which(grid_cell_NNs_w_burn_delay$burn_delay == min(grid_cell_NNs_w_burn_delay$burn_delay, na.rm=TRUE)),
    #                                                  "locnum"]
    active_fire_grid_not_found_NNs <- grid[!grid$found_NNs & grid$burning,]
    fire_igntion_cell_df <- active_fire_grid_not_found_NNs[which(active_fire_grid_not_found_NNs$burn_delay == min(active_fire_grid_not_found_NNs$burn_delay, na.rm=TRUE))[1],]
    
    active_fire_grid <- grid[!grid$burning,]  
    
    fire_igntion_cell <- fire_igntion_cell_df$locnum
    current_burn_time <- fire_igntion_cell_df$burn_delay
    
    
  }
  
  return(grid)
  
}

# where does fire start?
this_fire_igntion_cell <- 18
this_grid <- this_grid_shp@data


this_grid_sim <- run_burn_simulation(this_grid, this_fire_igntion_cell)

##### VISUALISE BURN SIMULATION #########
#########################################
max(this_grid_sim$burn_delay)

take_burn_sim_snapshot <- function(grid, time){
  grid[grid$burn_delay < time,] 
}  

this_snapshot_times <- matrix(1:9*rep(10, 9))

this_grid_sim_snapshots <- apply(this_snapshot_times, 1, function(x) take_burn_sim_snapshot(this_grid_sim, x))

fire_pal <- colorNumeric(c("blue", "red"), c(TRUE, FALSE))
fire_map_list <- list()
for (i in 1:length(this_grid_sim_snapshots)){
  
  print(i)
  this_grid_shp@data$on_fire <- this_grid_shp@data$locnum %in% this_grid_sim_snapshots[[i]]$locnum
  shp_df <- broom::tidy(this_grid_shp, region = "on_fire")
  fire_map_list[[i]] <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group), colour = "black", fill = NA)
  
}

plot_grid(plotlist = fire_map_list)
