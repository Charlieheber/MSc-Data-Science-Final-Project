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
  # MC_1000hr_week = this_MC_1000hr_week
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

