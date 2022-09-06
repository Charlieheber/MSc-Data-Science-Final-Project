#### EQUILIBRIUM MOISTURE CONTENT
#################################
# EMC is equilibirum moisture content
# RH is relative humidity
# TEMP is a dry bulb temperature

# Relative humidity < 10%
EMC = 0.03229 + 0.281073 * RH - 0.000578 * TEMP * RH # 1a

# Relative Humidity Equal to or Greater Than 10%
# but Less Than 50%
EMC = 2.22749 + 0.160107 * RH - 0.014784 * TEMP # 1b

# Relative Humidity Equal to or Greater Than 50 Percent
EMC = 21.0606 + 0.005565 * RH ** 2 - 0.00035 * RH * TEMP - 0.483199 * RH # 1c

# get equilibrium moisture content
get_EMC <- function(RH, TEMP){
  
  if(RH < 0.1){ # Relative humidity < 10%
    
    EMC = 0.03229 + 0.281073 * RH - 0.000578 * TEMP * RH # 1a
  
  } else if(RH < 0.5){ # Relative Humidity Equal to or Greater Than 10%, but Less Than 50%
    
    EMC = 2.22749 + 0.160107 * RH - 0.014784 * TEMP # 1b    
  
  } else if(RH > 0.5){ # Relative Humidity Equal to or Greater Than 50 Percent

    EMC = 21.0606 + 0.005565 * RH ** 2 - 0.00035 * RH * TEMP - 0.483199 * RH # 1c
    
  }
  
  return(EMC)
  
}

# With these equations, the EMC's can be evaluated for 
# (1) observation time 
# (2) the time of maximum temperature-minimum relative humidity
# (3) the time of minimum temperature-maximum relative humidity

# EMCOBS = f(TMPOBS, RHOBS)
# EMCMIN = f(TMPMAX, RHMIN)
# EMCMAX = f(TMPMIN, RHMAX) 

# in which
# TMPOBS is the dry bulb temperature at the afternoon observation time
# TMPMIN is the 24-hour minimum dry bulb temperature
# TMPMAX is the 24-hour maximum dry bulb temperature
# RHOBS is the relative humidity at the afternoon observation time
# RHMIN is the 24-hour minimum relative humidity
# RHMAX is the 24-hour maximum relative humidity

# FUELS: 1 HOUR TIMELAG ############
####################################

# The response of 1-hour timelag fuels to changes in the environmental conditions is so rapid that only the potential
# moisture content, which is equivalent to the EMC at the fuel-atmosphere interface, is required. The first task is to
# estimate the relative humidity and dry bulb temperature of the air in immediate contact with the fuel elements
# (TMPPRM and RHPRM). The approach, based on work by Byram and Jemison (1943), consists of correcting the
# dry bulb temperature and relative humidity values existing at instrument height (4.5 ft) according to the intensity of
# the insolation (amount of sunshine). Time of year or variables affecting insolation other than cloudiness are not considered. The amount of cloudiness is indicated by the stateof-weather code. The temperature correction is added (° F);
# the relative humidity correction is a multiplier

# State-of-weather code Temperature Relative humidity
#         0                + 25         * 0.75
#         1                + 19         * 0.83
#         2                + 12         * 0.92
#         3                + 5          * 1.00

## State of the weather code defintions
## https://www.researchgate.net/figure/Definition-of-state-of-weather-SOW-codes-based-on-daily-precipitation-PPT-and_tbl2_329025793

# SOW  Description           PPT            %RSDS_max
#  0     Clear              <0.01             >= 91
#  1     Scattered Clouds   <0.01         73<%RSDS_max<91
#  2     Broken Clouds      <0.01         50<%RSDS_max<73
#  3     Overcast           <0.01         50<%RSDS_max<73
#  4     Fog                ---                --- 
#  5     Drizzle            0.01<PPT<0.05      ---
#  6     Rain               >= 0.05            ---

get_state_of_weather_code(RSDS, RSDS_max, daily_PPT_inches){
  
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
  
}

# Boundary Layer EMC:
#   From equations (1a), (1b), or (1c)

# EMCPRM = f(TMPPRM, RHPRM) 

# When Fuel Moisture Sticks Are Not Used:
# MC1 = 1.03 * EMCPRM
# When Fuel Moisture Sticks Are Used:
# MC1 = (4.0 * EMCPRM + MC10) / 5.0
# in which MC10 is the 10-hour timelag fuel moisture. 

# If It Is Raining at the Afternoon Observation Time:
# MC1 = 35.0 
  
get_MC_1hr <- function(RH, TEMP, state_of_weather_code, fuel_moisture_sticks_used, raining_afternoon, MC_10_hr=NULL){
  
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
  
  } else if(raining_afternoon){
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

# If an Observation Is Being Processed and Fuel Sticks
# Are Not Being Used:
  
# MC10 = 1.28 * EMCPRM 

get_MC_10hr <- function(RH, TEMP){
  
  EMCPRM <- get_EMC(RH, TEMP)
  
  MC_10hr <- 1.28 * EMCPRM
  
  return(MC_10hr)
}

# FUELS: 100 HOUR TIMELAG ##########
####################################
# Because of the slow response of the 100-hour and the
# 1000-hour classes of fuels to changes in environmental
# conditions, we use an EMC that represents the average
# drying-wetting potential of the atmosphere for the preceding 24-hour period. The 24-hour average EMC is
# denoted as EMCBAR, a weighted average of EMCMAX
# and EMCMIN. Weighting is done on the basis of hours of
# daylight and hours of darkness that are functions of latitude and date

# Duration of Daylight:

# PHI = LAT * 0.01745

# in which
# LAT is the station latitude in degrees.
# DECL = 0.41008 * SIN((JDATE-82) * 0.01745) 

# in which
# JDATE is the Julian date.
# DECL is the solar declination in radians.
# DAYLIT = 24 * (1. - ACOS(TAN(PHI)
#                          * TAN(DECL))/3.1416)


# in which DAYLIT is the number of hours between sunrise
# and sunset. 

# Weighted 24-Hour Average EMC:
# EMCBAR = (DAYLIT * EMCMIN + (24.0 - DAYLIT) * EMCMAX) / 24.0
# Weighted 24-Hour Average Boundary Condition:
# BNDRYH = ((24.0 - PPTDUR) * EMCBAR + PPTDUR * (0.5 * PPTDUR + 41.0)) / 24.0 

# in which PPTDUR is the hours of precipitation reported
# (predicted) for the 24-hours.

# 100-Hour Timelag Fuel Moisture: The model used in the
# manual version of the 1978 NFDRS to calculate the 100-
# hour timelag fuel moisture differs from this model in two
# ways: 
# (1) daylength is not considered, and 
# (2) the 24-hour average EMC is a function of the simple averages of the
# 24-hour temperature and relative humidity extremes.

# MC100 = YMC100 + (BNDRYH - YMC100) * (1.0 - 0.87 * EXP(-0.24))

# in which YMC100 is the MC100 value calculated the previous day.

# Initializing YMC100 at the Beginning of a Computational Period:
#   YMC100 = 5.0 + (5.0 * CLIMAT)


get_daylight_hours <- function(LAT, JDATE){
  
  # transform to radians
  PHI <- LAT * (pi/180)
  
  # solar delination 
  DECL <- 0.41008*sin(JDATE-82)*(pi/180)
  
  DAYLIGHT_hours <- 24*(1 - acos(tan(PHI)*tan(DECL))/pi)
  
  return(DAYLIGHT_hours)
}

get_MC_100hrs <- function(DAYLIGHT_hours, EMC_min, EMC_max, PPT_24hours, YMC100){
  
  EMC_bar = (DAYLIGHT_hours * EMC_min + (24.0 - DAYLIGHT_hours) * EMC_max) / 24.0 
  
  BNDRYH = ((24.0 - PPT_24hours) * EMC_bar + PPT_24hours * (0.5 * PPT_24hours + 41.0)) / 24.0
  
  MC_100hrs = YMC100 + (BNDRYH - YMC100) * (1.0 - 0.87 * exp(-0.24)) 
  
  return(MC_100hrs)
  
}

