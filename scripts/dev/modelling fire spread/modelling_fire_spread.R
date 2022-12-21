rm(list=ls())
library(here)

##### LOAD PACKAGES ######################
##########################################

script_loc <- paste0(here::here(), "/scripts/R/")
req_packages = c("general",  "spatial", "visualisation")
source(paste0(script_loc, "libraries_and_file_locs.R"))

#### BUILD GRID SHAPEFILE ################
##########################################
this_n_cols <- 24
this_n_rows <- 24
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

this_grid_shp$grid_length <- 10
this_fire_igntion_cell <- 220

#### BUILD FUNCTION FOR FINDING NEAREST #
#### NEIGHBOURS #########################

# where does fire start?
this_grid_1 <- this_grid_shp@data
this_grid_shp_1 <- this_grid_shp

this_grid_2 <- this_grid_shp@data
this_grid_shp_2 <- this_grid_shp

this_grid_2.2 <- this_grid_shp@data
this_grid_shp_2.2 <- this_grid_shp

this_grid_3 <- this_grid_shp@data
this_grid_shp_3 <- this_grid_shp

# SCENARIO 1 - UNIFORM SPREAD RATE
this_grid_1$fire_spread_rate <- 1 # uniform spread rate
this_grid_1$fuel_type <- "fueltype_1" # uniform spread rate
this_grid_shp_1@data <- this_grid_1

# SCENARIO 2 - LOW ROS TO HIGH ROS
this_grid_2$fire_spread_rate <- 1
this_grid_2$fuel_type <- "fueltype_1"

this_grid_2[this_grid_2$colNum > 11,]$fire_spread_rate <- 4
this_grid_2[this_grid_2$colNum > 11,]$fuel_type <- "fueltype_2"

this_grid_shp_2@data <- this_grid_2

# SCENARIO 2.2 - HIGH ROS TO LOW ROS
this_grid_2.2$fire_spread_rate <- 4
this_grid_2.2$fuel_type <- "fueltype_2"

this_grid_2.2[this_grid_2$colNum > 11,]$fire_spread_rate <- 1
this_grid_2.2[this_grid_2$colNum > 11,]$fuel_type <- "fueltype_1"

this_grid_shp_2.2@data <- this_grid_2.2

# SCENARIO 3 - URBAN CENTRE
this_grid_3$fire_spread_rate <- 1
this_grid_3$fuel_type <- "fueltype_1"

this_grid_3[which(this_grid_3$colNum %in% 10:15 &
                    this_grid_3$rowNum %in% 10:15 | 
                    this_grid_3$locnum %in% c(203:206, 256, 280, 304, 328, 371:374, 249, 273, 297, 321)),]$fire_spread_rate <- 0.1

this_grid_3[which(this_grid_3$colNum %in% 10:15 &
                    this_grid_3$rowNum %in% 10:15 | 
                    this_grid_3$locnum %in% c(203:206, 256, 280, 304, 328, 371:374, 249, 273, 297, 321)),]$fuel_type <- "urban"

this_grid_shp_3@data <- this_grid_3

# VISUALISE
col_pal <- colorFactor(
  c("lightgreen", "darkgreen", "grey"), domain=c("fueltype_1", "fueltype_2", "urban"), ordered=TRUE
)

leaflet() %>%
  addPolygons(data=this_grid_shp_1, fillColor = ~col_pal(fuel_type), fillOpacity = 1, color="grey", group="1") %>%
  addPolygons(data=this_grid_shp_2, fillColor = ~col_pal(fuel_type), fillOpacity = 1, color="grey", group="2") %>%
  addPolygons(data=this_grid_shp_2.2, fillColor = ~col_pal(fuel_type), fillOpacity = 1, color="grey", group="2.2") %>%
  addPolygons(data=this_grid_shp_3, fillColor = ~col_pal(fuel_type), fillOpacity = 1, color="grey", group="3", label=~locnum) %>%
  addLayersControl(overlayGroups = c("1", "2", "2.2", "3"))


#### RUN BURN SIMULATIONS ###############
#########################################

this_grid_sim_1 <- run_burn_simulation(this_grid_1, 276)
this_grid_sim_2 <- run_burn_simulation(this_grid_2, this_fire_igntion_cell)
this_grid_sim_2.2 <- run_burn_simulation(this_grid_2.2, this_fire_igntion_cell)
this_grid_sim_3 <- run_burn_simulation(this_grid_3, this_fire_igntion_cell)

### VISUALISE ###########################
#########################################

this_grid_sim_shp <- this_grid_shp

this_grid_1_snapshots <- stats::quantile(this_grid_sim_1$burn_delay,probs=seq(0,1, by=1/6))
this_grid_2_snapshots <- stats::quantile(this_grid_sim_2$burn_delay,probs=seq(0,1, by=1/6))
this_grid_2.2_snapshots <- stats::quantile(this_grid_sim_2.2$burn_delay,probs=seq(0,1, by=1/6))
this_grid_3_snapshots <- stats::quantile(this_grid_sim_3$burn_delay,probs=seq(0,1, by=1/6))

max(this_grid_sim_shp$burn_delay)

this_grid_sim_shp@data <- this_grid_sim_3
this_grid_snapshots <- this_grid_3_snapshots
leaflet(this_grid_sim_shp) %>%
  addPolygons(fillColor = ~col_pal(fuel_type), fillOpacity = 1, color="grey", group="1") %>%
  addPolygons(data=this_grid_sim_shp[which(this_grid_sim_shp$burning & this_grid_sim_shp$burn_delay < this_grid_snapshots[2]/2),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_0") %>%
  addPolygons(data=this_grid_sim_shp[which(this_grid_sim_shp$burning & this_grid_sim_shp$burn_delay < this_grid_snapshots[3]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_1") %>%
  addPolygons(data=this_grid_sim_shp[which(this_grid_sim_shp$burning & this_grid_sim_shp$burn_delay < this_grid_snapshots[4]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_2") %>%
  addPolygons(data=this_grid_sim_shp[which(this_grid_sim_shp$burning & this_grid_sim_shp$burn_delay < this_grid_snapshots[5]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_3") %>%
  addPolygons(data=this_grid_sim_shp[which(this_grid_sim_shp$burning & this_grid_sim_shp$burn_delay < this_grid_snapshots[6]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_4") %>%
  addPolygons(data=this_grid_sim_shp[which(this_grid_sim_shp$burning & this_grid_sim_shp$burn_delay < this_grid_snapshots[7]),],
              fillOpacity=0.8, opacity=0,
              fillColor="red", color="black",
              group = "fire_5") %>%
  addLayersControl(overlayGroups = c("fire_0", "fire_1", "fire_2", "fire_3", "fire_4", "fire_5", "fire_6"))


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
  fire_map_list[[i]] <- ggplot() + geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, color=group), colour = "black", fill = NA)
  
}

plot_grid(plotlist = fire_map_list)



this_grid_snapshots <- stats::quantile(this_grid_sim$burn_delay,probs=seq(0,1, by=1/6))

this_LANDFIRE_shp_fuel_map <- sp::merge(this_LANDFIRE_shp, this_grid_sim[, c("locnum", "burning", "burn_delay")], by="locnum")

# fuel type key
colours <- c("grass" = "forestgreen", 
             "shrub" = "darksalmon",
             "timber_litter" = "burlywood4",
             "slash" = "purple",
             "urban" = "grey",
             "snow" = "white",
             "agriculture" = "darkgoldenrod1",
             "water" = "blue",
             "barren" = "darkorange4")

fuel_pal <- colorFactor(
  palette = colours,
  domain = names(colours),
  ordered = TRUE
)

this_LANDFIRE_shp_fuel_map@data











