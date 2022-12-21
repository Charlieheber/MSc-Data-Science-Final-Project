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
run_burn_simulation <- function(grid, fire_ignition_cell, fire_size_grid_sqs){
  
  # grid <- this_LANDFIRE_mod_shp@data
  # fire_ignition_cell <- this_LANDFIRE_EVENT$locnum
  # fire_size_grid_sqs <- this_EVENT$FIRE_SIZE_grid_sq
  
  # grid <- this_LANDFIRE_mod_shp@data
  # fire_ignition_cell <- this_LANDFIRE_EVENT$locnum
  # fire_size_grid_sqs <- this_EVENT$FIRE_SIZE_grid_sq
  # grid <- this_grid
  # fire_igntion_cell <- 44
  
  # add res columns
  grid$burn_delay <- 0
  grid$burning <- FALSE
  grid$found_NNs <- FALSE
  
  # add first cell
  grid[grid$locnum == fire_ignition_cell, "burning"] <- TRUE
  head(grid)
  
  # initiate active fire grid (so any unburnt cells)
  active_fire_grid <- grid
  
  # initiate burn time
  current_burn_time <- 0
  
  # WHILE LOOP START HERE
  # WHILE LOOP START HERE
  while(sum(grid$burning) < fire_size_grid_sqs+1){
    
    Sys.sleep(0.1)
    message(paste("active fire cell:", fire_ignition_cell))
    
    # fire nearest neighbours
    grid_cell_NNs <- find_nearest_neighbours(active_fire_grid, grid[grid$locnum == fire_ignition_cell,])
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
    # grid_w_current_burn_delay[grid_w_current_burn_delay$locnum == fire_ignition_cell, "found_NNs"] <- TRUE
    grid[grid$locnum == fire_ignition_cell, "found_NNs"] <- TRUE
    
    
    grid$burn_delay_current <- NULL
    
    # start with new fire cell   
    # fire_ignition_cell <- grid_cell_NNs_w_burn_delay[which(grid_cell_NNs_w_burn_delay$burn_delay == min(grid_cell_NNs_w_burn_delay$burn_delay, na.rm=TRUE)),
    #                                                  "locnum"]
    active_fire_grid_not_found_NNs <- grid[!grid$found_NNs & grid$burning,]
    fire_ignition_cell_df <- active_fire_grid_not_found_NNs[which(active_fire_grid_not_found_NNs$burn_delay == min(active_fire_grid_not_found_NNs$burn_delay, na.rm=TRUE))[1],]
    
    active_fire_grid <- grid[!grid$burning,]  
    
    fire_ignition_cell <- fire_ignition_cell_df$locnum
    current_burn_time <- fire_ignition_cell_df$burn_delay
    
  }
  
  return(grid)
  
}
