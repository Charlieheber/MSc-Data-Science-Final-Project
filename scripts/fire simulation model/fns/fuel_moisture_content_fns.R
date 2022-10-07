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
# RH is relative humidity
# TEMP is a dry bulb temperature

# Relative humidity < 10%
# EMC = 0.03229 + 0.281073 * RH - 0.000578 * TEMP * RH # 1a

# Relative Humidity Equal to or Greater Than 10%
# but Less Than 50%
# EMC = 2.22749 + 0.160107 * RH - 0.014784 * TEMP # 1b

# Relative Humidity Equal to or Greater Than 50 Percent
# EMC = 21.0606 + 0.005565 * RH ** 2 - 0.00035 * RH * TEMP - 0.483199 * RH # 1c

# get equilibrium moisture content
# RH is given as decimal
# TEMP in fahrenheit
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
#  3     Overcast           <0.01         50>=%RSDS_max
#  4     Fog                ---                --- 
#  5     Drizzle            0.01<PPT<0.05      ---
#  6     Rain               >= 0.05            ---

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

# Weighted 24 Hour Average Boundary Condition:
#   BNDRYT = ((24.0 - PPTDUR) * EMCBAR +
#               PPTDUR * (2.7 * PPTDUR + 76.0)) / 24.0
# Seven Day Running Average Boundary Condition:
#   BDYBAR = (BNDRYT(1) + ...............+ BNDRYT(7)) / 7.0
# in which ( ) denotes a day in the 7-day series. It is necessary,
# therefore, to maintain a 1 x 7 array of BNDRYT values. 

# where:
#   PPTDUR is Duration of precipitation
#   EMCBAR is Average EMC, weighted by hours of day and night

get_BNDRY_T <- function(DAYLIGHT_hours, EMC_min, EMC_max, PPT_duration){
  
  EMC_bar <- (DAYLIGHT_hours * EMC_min + (24.0 - DAYLIGHT_hours) * EMC_max) / 24.0 
  
  BNDRY_T <- ((24.0 - PPT_duration) * EMC_bar + PPT_duration * (2.7 * PPT_duration + 76.0)) / 24.0
  
  return(BNDRY_T)
}

# Precipitation--In pre-1972 fire-weather reports, precipitation duration (PPTDUR) was not reported, but precipitation amount (PPTAMT) was. So by assuming a rainfall
# rate (WETRAT), a pseudo-duration can be calculated as
# follows:
#   PPTDUR = IRND((PPTAMT / WETRAT) + 0.49)
# in which
# IRND indicates that the quantity in parentheses is a
# rounded integer.
# PPTDUR cannot be greater than 8 hours.
# WETRAT is a function of climate class as follows: 

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


# FUELS: LIVE HERBACEOUS ###########
####################################

# The loading of the herbaceous fuels is a fuel model parameter just as the 1-hour timelag fuel loading is a fuel
# model parameter. The user specifies the herbaceous type as annual or perennial and the NFDRS climate class of the
# observation site. Those parameters control the rate at which the model passes through these five stages:
# 1. pregreen (MCHERB 30 percent or less)
# 2. greenup
# 3. green (MCHERB greater than 120 percent)
# 4. transition (MCHERB 30 to 120 percent)
# 5. cured or frozen (MCHERB 30 percent or less)
# in which MCHERB is the moisture content of the herbaceous plants. 

# 1) PREGREEN: LATE SUMMER/FALL
#   Moisture content equal to 1hour dead fuel MC

# 2) GREENUP: SPRING (declared by user)
#   MC_herb intially at 30% which increases to 120%.
#   Length of greenup period determined by NFDRS climate class 
#   1 week for class 1, 2 weeks for class 2, 3 weeks for class 3 and 4 weeks for class 4

# 3) GREEN: WHEN MC_herb reached 120% 
#   perennials MC increases of decreases as available MC increases or decreases
#   annuals can only remain the same of decrease when greenup process is complete
#   maximum value of MC_herb in 250%

# 4) TRANSITION: WHEN MC_herb falls below 120% 
#   herbaceous loading transferred to 1hr fuel class (reverse of greenup)
#   when MC_herb falls below 30%,loading of herbaceous fuels is 0.
#   perennials plants can pick up moisture, annuals cannot.

# 5) CURED: When MC_herb falls below 30%
#   perennials can regreen in MC increases
#   annuals stay cured until user declares GREEN

# in more mathsy terms...

get_MC_herb <- function(MC_1hr, MC_1000hr, MC_1000hr_previous_day, MC_herb_pregreen=NULL,
                        pregreen_DOY, greenup_DOY, curing_DOY, DOY, CLIMAT,
                        TEMP_min, TEMP_max, annuals){
  
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
    
    message("pregreen stage detected")
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
      message("greenup stage detected")
      MC_herb <- MC_herb_pregreen + (MC_herbp - MC_herb_pregreen)*GREN
      
      return(c("MC_herb" = MC_herb))
      
    } else if(GREN >= 1 & MC_herbp > 120){
      
      message("green stage detected")
      GREN = 1 # greenup duration defined as 7xclimate_class
      
      MC_herb <- MC_herbp
      
      if(MC_herb >= 250) return(c("MC_herb" = 250)) # MC herb cannot get larger than 250 during greenup or green periods

      return(c("MC_herb" = MC_herb))
      
    } else if(DOY < curing_DOY){
      
      message("transition stage detected")
      
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
      
      message("curing stage detected")
      
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



# during greenup
# DIFF <- MC_1000hr - YM1000
# X1000 <- YX1000 + DIFF*KWET*KTMP 

# YM1000 is MC1000 of previous day
# YX1000 is X1000 of previous day
# DIFF is 24hr change in MC1000
# KWET is wetting factor
# KTMP is temp factor

# in which:
#   KWET:
#   if MC_1000hr > 25%, KWET = 1
#   if MC_1000hr < 25% & MC_1000hr > 9%, KWET = (0.0333*MC1000hr + 0.1675)
#   if MC_1000hr < 10, KWET = 0.5
#   if DIFF <= 0, KWET = 1
#   KTMP:
#   if (TMP_max + TMP_min)/2 <= 50 degrees F, KTMP = 0.6
#   otherwise KTMP = 1

# Next needed is the moisture content that herbaceous
# fuels would have if the greenup period were over; this we
# call MCHRBP (P for Potential). MCHRBP is linearly
# related to X 1000, but the constants of the relationship are
# functions of the NFDRS climate class. 

# MC_herbp = GA_herb + GB_herb * X1000

# NFDRS Climate class: HERBGA HERBGB
# 1                    -70.0 12.8
# 2                    -100.0 14.0
# 3                    -137.5 15.5
# 4                    -185.0 17.4

# The length of the greenup period, in days, is seven times
# the NFDRS climate class. The fraction of greenup period
# that has elapsed must be calculated so that the loading of
# the herbaceous fuel can be calculated.

# GREN = GRNDAY / (7.0 * CLIMAT) 

# in which GRNDAY is the number of days since the greenup
# sequence was started. 



























