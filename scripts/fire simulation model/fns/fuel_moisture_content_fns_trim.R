#### FAHRENHEIT to CELSIUS #####
################################

fahrenheit_to_celsius <- function(temp_f){
  
  temp_c <- (temp_f - 32)*(5/9)
  
  return(temp_c)
}

celsius_to_fahrenheit <- function(temp_c){
  
  temp_f <- (temp_c * 9/5) + 32
  
  return(temp_f)
  
}


#### EQUILIBRIUM MOISTURE CONTENT
#################################
# EMC is equilibirum moisture content
# RH is relative humidity (decimal)
# TEMP is a dry bulb temperature (fahrenheit)
get_EMC <- function(RH, TEMP){
  
  if(RH > 1) return(message("RH greater than 1, should be given as decimal"))
  
  RH <- RH*100
  
  if(RH < 10){ # Relative humidity < 10%
    
    EMC = 0.03229 + 0.281073*RH - 0.000578*TEMP*RH # 1a
    
  } else if(RH < 50){ # Relative Humidity Equal to or Greater Than 10%, but Less Than 50%
    
    EMC = 2.22749 + 0.160107*RH - 0.014784*TEMP # 1b    
    
  } else if(RH >= 50){ # Relative Humidity Equal to or Greater Than 50 Percent
    
    EMC = 21.0606 + 0.005565*RH^(2) - 0.00035*RH*TEMP - 0.483199*RH # 1c
    
  }
  
  return(EMC)
  
}

# FUELS: 1 HOUR TIMELAG ############
####################################

get_state_of_weather_code <- function(RSDS, RSDS_max, daily_PPT_inches){
  
  if(daily_PPT_inches < 0.01){
    
    # clear
    if(RSDS/RSDS_max >= 0.91) state_of_weather = 0
    
    # scattered clouds
    if(RSDS/RSDS_max < 0.91 & RSDS/RSDS_max >= 0.73) state_of_weather = 1
    
    # broken clouds
    if(RSDS/RSDS_max < 0.73 & RSDS/RSDS_max > 0.50) state_of_weather = 2
    
    # overcast
    if(RSDS/RSDS_max <= 0.50) state_of_weather = 3
    
  }
  
  # drizzle
  if(daily_PPT_inches > 0.01 & daily_PPT_inches < 0.05) state_of_weather = 5
  
  # rain
  if(daily_PPT_inches > 0.05) state_of_weather = 6
  
  return(state_of_weather)
  
}

# RH is given as decimal
# TEMP in fahrenheit
get_MC_1hr <- function(RH, TEMP, state_of_weather_code, fuel_moisture_sticks_used, MC_10_hr=NULL){
  
  # correction of EMC to EMC at fuel-moisture interface
  if(state_of_weather_code == 0){
    RH_correction <- RH*0.75
    TEMP_correction <- TEMP + 25
    
  } else if(state_of_weather_code == 1){
    RH_correction <- RH*0.83
    TEMP_correction <- TEMP + 19
    
  } else if(state_of_weather_code == 2){
    RH_correction <- RH*0.92
    TEMP_correction <- TEMP + 12
    
  } else if(state_of_weather_code == 3){
    RH_correction <- RH*1.00
    TEMP_correction <- TEMP + 5
    
  } else if(state_of_weather_code == 5 | state_of_weather_code == 6){
    MC_1hr <- 35.0
    return(MC_1hr)
  } 
  
  EMCPRM <- get_EMC(RH_correction, TEMP_correction)
  
  # are fuel moisture sticks used
  if(fuel_moisture_sticks_used){
    MC_1hr <- (4.0 * EMCPRM + MC10) / 5.0
  } else{
    MC_1hr <- 1.03 * EMCPRM
  }
  
  return(MC_1hr)
  
}


# FUELS: 10 HOUR TIMELAG ###########
####################################

get_MC_10hr <- function(RH, TEMP, state_of_weather_code){
  
  # correction of EMC to EMC at fuel-moisture interface
  if(state_of_weather_code == 0){
    RH_correction <- RH*0.75
    TEMP_correction <- TEMP + 25
    
  } else if(state_of_weather_code == 1){
    RH_correction <- RH*0.83
    TEMP_correction <- TEMP + 19
    
  } else if(state_of_weather_code == 2){
    RH_correction <- RH*0.92
    TEMP_correction <- TEMP + 12
    
  } else if(state_of_weather_code == 3){
    RH_correction <- RH*1.00
    TEMP_correction <- TEMP + 5
    
  } else if(state_of_weather_code == 5 | state_of_weather_code == 6){
    MC_1hr <- 35.0
    return(MC_1hr)
  } 
  
  
  EMCPRM <- get_EMC(RH_correction, TEMP_correction)
  
  MC_10hr <- 1.28 * EMCPRM
  
  return(MC_10hr)
}

# FUELS: 100 HOUR TIMELAG ##########
####################################

# Latitude in degrees
# JDATE is number day of year (1-365) 
get_daylight_hours <- function(LAT, JDATE){
  
  # transform to radians
  PHI <- LAT * (pi/180)
  
  # solar declination in radians
  DECL <- 0.41008*sin((JDATE-82)*(pi/180))
  
  DAYLIGHT_hours <- 24*(1 - acos(tan(PHI)*tan(DECL))/pi)
  
  return(DAYLIGHT_hours)
}

# where YMC_100hrs is MC from previous day
get_MC_100hr <- function(DAYLIGHT_hours, EMC_min, EMC_max, PPT_24hours, YMC_100hrs, initialize_YMC_100hrs=FALSE, CLIMAT=NULL){
  
  if(initialize_YMC_100hrs) YMC_100hrs = 5.0 + (5.0 * CLIMAT)
  
  EMC_bar = (DAYLIGHT_hours * EMC_min + (24.0 - DAYLIGHT_hours) * EMC_max) / 24.0 
  
  BNDRYH = ((24.0 - PPT_24hours) * EMC_bar + PPT_24hours * (0.5 * PPT_24hours + 41.0)) / 24.0
  
  MC_100hrs = YMC_100hrs + (BNDRYH - YMC_100hrs) * (1.0 - 0.87 * exp(-0.24)) 
  
  return(MC_100hrs)
  
}


# FUELS: 1000 HOUR TIMELAG #########
####################################

#'
#' PPT_duration is Duration of precipitation
#' EMC_min is min EMC for day
#' EMC_max is max EMC for day
get_BNDRY_T <- function(DAYLIGHT_hours, EMC_min, EMC_max, PPT_duration){
  
  EMC_bar <- (DAYLIGHT_hours * EMC_min + (24.0 - DAYLIGHT_hours) * EMC_max) / 24.0 
  
  BNDRY_T <- ((24.0 - PPT_duration) * EMC_bar + PPT_duration * (2.7 * PPT_duration + 76.0)) / 24.0
  
  return(BNDRY_T)
}

# PPT_24hours is precipitation in inches over a day
# WETRAT is the rate of rainfall in inches per hour
# listed as 0.25 in areas of climate class 1 or 2
get_PPT_duration <- function(PPT_24hours, WETRAT){
  
  PPT_duration = ceiling((PPT_24hours / WETRAT))
  
  if(PPT_duration > 8) PPT_duration = 8 
  
  return(PPT_duration)
}


# BNNDRY_week an array of BNDRY_T for each day in a week
# MC_1000_hr_week is an array of 1000hr moisture content from the last 7 days  
get_MC_1000hr <- function(BNDRY_week, MC_1000hr_week, initialize_MC_1000hrs=FALSE, CLIMAT=NULL, verbose=FALSE){
  
  # initialize_MC_1000hrs = TRUE
  # BNDRY_week = this_BNDRY_week
  # MC_1000hr_week = MC_1000hr_week_new
  # initialize_MC_1000hrs = this_initialize_MC_1000hrs
  # CLIMAT = this_daily_station_data$CLIMAT
  
  if(initialize_MC_1000hrs){
    MC_1000hr_week <- rep(1,7)*(10.0 + (5.0 * CLIMAT))
    BNDRY_week <- rep(1,7)*(10.0 + (5.0 * CLIMAT))
  } 
  
  BNDRY_bar <- mean(BNDRY_week)
  
  MC_1000hr <- MC_1000hr_week[1] + (BNDRY_bar - MC_1000hr_week[1])*(1-0.82*exp(-0.168))
  
  if(verbose) message(paste("moisture content (1000hrs):", round(MC_1000hr,3)))
  
  MC_1000hr_week_new <- MC_1000hr_week[2:7]
  MC_1000hr_week_new[7] <- MC_1000hr
  
  return(list("MC_1000hr_week" = MC_1000hr_week_new, "MC_1000hr" = MC_1000hr))
  
}

# FUELS: LIVE HERBACEOUS ###########
####################################

#'
#' MC_1hr moisture content 1hour dead fuel
#' MC_1000hr moisture content 1000 hour dead fuel on the previous day
#' MC_herb_pregreen moisture content day before green up stage
#' pregren_DOY day of year of pregreen stage
#' greenup_DOY day of year of greenup stage
#' curing_DOY day of year of curing stage
#' DOY day of year
#' CLIMAT climate class
#' TEMP_min min daily temp
#' TEMP_max max daily temp
#' annuals annuals or perrenials
#'
get_MC_herb <- function(MC_1hr, MC_1000hr, MC_1000hr_previous_day, MC_herb_pregreen=NULL,
                        pregreen_DOY, greenup_DOY, curing_DOY, DOY, CLIMAT,
                        TEMP_min, TEMP_max, annuals, verbose=FALSE){
  
  # this_daily_station_data <- this_station_data[1,]
  # MC_1hr = this_daily_station_data$MC_1hr
  # MC_1000hr = this_daily_station_data$MC_1000hr
  # MC_1000hr_previous_day = this_yesterday_station_data$MC_1000hr
  # MC_herb_pregreen= 20
  # pregreen_DOY = this_pregreen_DOY
  # greenup_DOY = this_greenup_DOY
  # curing_DOY = this_curing_DOY
  # DOY = this_daily_station_data$day_of_year
  # CLIMAT = this_daily_station_data$CLIMAT
  # TEMP_min = celsius_to_fahrenheit(this_daily_station_data$min_air_temp)
  # TEMP_max = celsius_to_fahrenheit(this_daily_station_data$max_air_temp)
  # annuals = FALSE
  
  # setup so pregreen_DOY is day 0
  DOY <- DOY - pregreen_DOY  
  greenup_DOY <-  greenup_DOY - pregreen_DOY
  # pregreen_DOY <- 0
  
  # PREGREEN ####################
  ###############################
  if(DOY < greenup_DOY){
    
    if(verbose) message("pregreen stage detected")
    MC_herb <- MC_1hr
    
    return(c("MC_herb" = MC_herb))
  } 
  
  # GREENUP/GREEN/TRANSITION/CURING ####
  #####################################
  if(DOY >= greenup_DOY){
    
    GRNDAY <- DOY - greenup_DOY # number of days since greenup has started
    
    # when greenup process begins these are the model settings
    if(GRNDAY == 0){
      MC_herb <- MC_herb_pregreen # set MC_herb to last pregreen MC
      X_1000 <- MC_1000hr # X1000 is independent variable in herbaceous fuel model (set to 1000hr MC)
      return(c("MC_herb" = MC_herb))
    }
    
    # set wetting and temp factors --
    # -------------------------------
    
    if(MC_1000hr > 25){
      KWET = 1
    } else if(MC_1000hr < 25 & MC_1000hr > 9){
      KWET = 0.0333*MC_1000hr + 0.1675
    } else if(MC_1000hr < 9){
      KWET = 0.5
    }
    
    DIFF <- MC_1000hr - MC_1000hr_previous_day
    
    if(DIFF <= 0){
      KWET = 1
    }
    
    if((TEMP_max + TEMP_min)/2 <= 50){
      KTEMP = 0.6
    } else KTEMP = 1
    
    # -------------------------------
    
    # independent variable in fuel moisture model
    X_1000 = MC_1000hr_previous_day + DIFF*KWET*KTEMP
    
    # set MC_herb potential ---------
    # -------------------------------
    
    if(CLIMAT == 1){
      GA_herb = -70
      GB_herb = 12.8
      
      ANN_ta = -150.5
      ANN_tb = 18.4
      PER_ta = 11.2
      PER_tb = 7.4
      
    } else if(CLIMAT == 2){
      GA_herb = -100
      GB_herb = 14
      
      ANN_ta = -187.7
      ANN_tb = 19.6
      PER_ta = -10.3
      PER_tb = 8.3
      
    } else if(CLIMAT == 3){
      GA_herb = -137.5
      GB_herb = 15.5
      
      ANN_ta = -245.2
      ANN_tb = 22.0
      PER_ta = -42.7
      PER_tb = 9.8
      
    } else if(CLIMAT == 4){
      GA_herb = -185
      GB_herb = 17.4
      
      ANN_ta = -305.2
      ANN_tb = 24.3
      PER_ta = -93.5
      PER_tb = 12.2
      
    }
    
    MC_herbp = GA_herb + GB_herb * X_1000
    
    # --------------------------------
    
    # fraction of greenup period that has elapsed
    GREN = GRNDAY / (7.0 * CLIMAT) 
    
    if(GREN < 1){
      if(verbose) message("greenup stage detected")
      MC_herb <- MC_herb_pregreen + (MC_herbp - MC_herb_pregreen)*GREN
      
      return(c("MC_herb" = MC_herb))
      
    } else if(GREN >= 1 & MC_herbp > 120){
      
      if(verbose) message("green stage detected")
      GREN = 1 # greenup duration defined as 7xclimate_class
      
      MC_herb <- MC_herbp
      
      if(MC_herb >= 250) return(c("MC_herb" = 250)) # MC herb cannot get larger than 250 during greenup or green periods
      
      return(c("MC_herb" = MC_herb))
      
    } else if(DOY < curing_DOY){
      
      if(verbose) message("transition stage detected")
      
      # For annuals:
      MC_herb = ANN_ta + ANN_tb * X_1000
      
      # For perennials:
      MC_herb = PER_ta + PER_tb * X_1000
      
      if(!annuals){
        
        if(MC_herb > 150) return(c("MC_herb" = 150))
        if(MC_herb < 30) return(c("MC_herb" = 30))
        
      } else{
        
        if(MC_herb > MC_herb_previous_day) return(c("MC_herb" = MC_herb_previous_day))
        
      }
      
      return(c("MC_herb" = MC_herb))
      
    } else if(DOY >= curing_DOY){ # CURING
      
      if(verbose) message("curing stage detected")
      
      if(annuals){
        
        MC_herb = MC_1hr
        
        return(c("MC_herb" = MC_herb))
        
      } else{
        
        MC_herb = PER_ta + PER_tb * X_1000
        
        if(MC_herb > 150) return(150)
        if(MC_herb < 30) return(30)
        
        return(c("MC_herb" = MC_herb))
      }
      
      
    }
    
  }
  
  
}

# FUELS: MC LIVE WOODY #################
########################################

# woody fuel moisture cannot be below 70%, 
# in accordance with NFDRS2016. 
# Verified w/ John Abatzoglou, implemented 3/16/2020


get_MC_wood_pregreen <- function(CLIMAT){
  
  # get MC wood pregreen
  if(CLIMAT == 1){
    MC_wood_pregreen <- 50
  } else if(CLIMAT == 2){
    MC_wood_pregreen <- 60
  } else if(CLIMAT == 3){
    MC_wood_pregreen <- 70
  } else if(CLIMAT == 4){
    MC_wood_pregreen <- 80
  }
  
  return(MC_wood_pregreen)
  
}

get_MC_wood <- function(DOY, pregreen_DOY, greenup_DOY, MC_1000hr, CLIMAT, MC_wood_pregreen,
                        MC_wood_previous_day){
  
  # setup so pregreen_DOY is day 0
  DOY <- DOY - pregreen_DOY  
  greenup_DOY <-  greenup_DOY - pregreen_DOY
  
  if(DOY < greenup_DOY){
    
    MC_wood <-get_MC_wood_pregreen(CLIMAT)
    
    if(MC_wood <= 70) MC_wood =70
    
    return(MC_wood) 
  }
  
  if(DOY >= greenup_DOY){
    
    GRNDAY <- DOY - greenup_DOY # number of days since greenup has started
    GREN <- GRNDAY / (7.0 * CLIMAT) 
    
    if(CLIMAT == 1){
      WOOD_ga <- 12.5
      WOOD_gb <- 7.5
    } else if(CLIMAT == 2){
      WOOD_ga <- -5
      WOOD_gb <- 8.2
    } else if(CLIMAT == 3){
      WOOD_ga <- -22.5
      WOOD_gb <- 8.9
    } else if(CLIMAT == 4){
      WOOD_ga <- -45
      WOOD_gb <- 9.8
    }
    
    
    if(GREN < 1){
      
      MC_wodp <- WOOD_ga + WOOD_gb * MC_1000hr
      
      if(MC_wood_previous_day >= MC_wood_pregreen){
        MC_wodi = MC_wood_previous_day 
      } else {
        MC_wodi = MC_wood_pregreen
      }
      
      MC_wood = MC_wodi + (MC_wodp - MC_wodi) * GREN 
      
      if(MC_wood <= 70) MC_wood =70
      
      return(MC_wood)
      
      
    } else if(GREN >= 1){
      
      MC_wood = WOOD_ga + WOOD_gb * MC_1000hr
      
      if(MC_wood < MC_wood_pregreen) {
        
        if(MC_wood <= 70) return(70)
        
        return(MC_wood_pregreen)
      }
      if(MC_wood > 200) return(200)
      
      if(MC_wood <= 70) MC_wood =70
      
      return(MC_wood)
      
    }
    
     
  } 
}












