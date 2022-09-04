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


# With these equations, the EMC's can be evaluated for 
# (1) observation time 
# (2) the time of maximum temperatureminimum relative humidity
# (3) the time of minimum temperature-maximum relative humidity

EMCOBS = f(TMPOBS, RHOBS)
EMCMIN = f(TMPMAX, RHMIN)
EMCMAX = f(TMPMIN, RHMAX) 

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

# Boundary Layer EMC:
#   From equations (1a), (1b), or (1c)

EMCPRM = f(TMPPRM, RHPRM) 

# When Fuel Moisture Sticks Are Not Used:
MC1 = 1.03 * EMCPRM
# When Fuel Moisture Sticks Are Used:
MC1 = (4.0 * EMCPRM + MC10) / 5.0
# in which MC10 is the 10-hour timelag fuel moisture. 

# If It Is Raining at the Afternoon Observation Time:
MC1 = 35.0 
  
  
  





