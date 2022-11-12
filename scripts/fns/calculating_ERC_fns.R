
#' Adapted from https://github.com/NCAR/fire-indices/blob/master/calc_just_erc.ncl
#' 
#' w1d,w10d,w100d,w1000d,wherb,wwood,depth,sg1d,sg10d,sg100d,sg1000d,sgherb,sgwood,extmoi,hd are read in according to the fuel model
#' fm1, fm10, fm100, fm1000 are the same as mc1, mc10, mc100, mc1000, the percent moisture content of 1-, 10-, 100-, 1000-hour timelag
#' erc is energy release component
#'
calculate_erc <- function(mcherb,fm1,fm10,fm100,fm1000,fmwood,fuel_model,fuel_models_vals){
  
  # mcherb = this_station_data_w_MCs$MC_herb[2]
  # fm1 = this_station_data_w_MCs$MC_1hr[2]
  # fm10 = this_station_data_w_MCs$MC_10hr[2]
  # fm100 = this_station_data_w_MCs$MC_100hr[2]
  # fm1000 = this_station_data_w_MCs$MC_1000hr[2]
  # fmwood = this_station_data_w_MCs$MC_wood[2]
  # fuel_model = "G"
  # fuel_models_vals = this_fuel_models_vals
  
  fuel_model_vals <- fuel_models_vals[,c("var", fuel_model), with=FALSE]  
  
  fuel_model_vals_lst <- setNames(split(fuel_model_vals[,fuel_model, with=FALSE], seq(nrow(fuel_model_vals))), 
                                  fuel_model_vals$var)
  
  # conversions from T/Ac to lbs/ft^2
  c1 = 0.046
  
  w1d = fuel_model_vals_lst$w1 * c1
  w10d = fuel_model_vals_lst$w10 * c1
  w100d = fuel_model_vals_lst$w100 * c1
  w1000d = fuel_model_vals_lst$w1000 * c1
  wherb = fuel_model_vals_lst$whrb * c1
  wwood = fuel_model_vals_lst$wwd * c1
  
  sg1d = fuel_model_vals_lst$s1
  sg10d = fuel_model_vals_lst$s10
  sg100d = fuel_model_vals_lst$s100
  sg1000d = fuel_model_vals_lst$s1000
  sgwood = fuel_model_vals_lst$swd
  sgherb = fuel_model_vals_lst$shrb
  hd = fuel_model_vals_lst$hd
  extmoi = fuel_model_vals_lst$emo
  depth = fuel_model_vals_lst$dp 
  
  stdl = 0.0555   # used in place of std and stl, since both have the same value
  rhodl = 32     # used in place of rhod and rhol, since both have the same value
  sdl = 0.01      # sd and sl
  etasdl = 0.174 * sdl^(-0.19)
  
  fctcur = 1.33 - 0.0111 * mcherb
  fctcur = fctcur
  fctcur = fctcur
  
  # clipping operation
  if(fctcur < 0) fctcur = 0
  if(fctcur > 1) fctcur = 1
  
  wherbc = fctcur * wherb # wherbc is never initialized as this in the MATLAB code, but is a useful intermediary variable
  w1dp = w1d + wherbc
  wherbp = wherb - wherbc # wherbp here is wherbc in MATLAB code
  wtotd = w1dp + w10d + w100d + w1000d
  wtotl = wwood + wherbp
  wtot = wtotd + wtotl
  
  # compute net fuel loading
  w1n = w1dp * (1. - stdl)
  w10n = w10d * (1. - stdl)
  w100n = w100d - (1. - stdl)
  wherbn = wherbp * (1. - stdl) # wherbn is from paper. In MATLAB code this is the variable whernc
  wwoodn = wwood * (1. - stdl)
  wtotln = wtotl * (1. - stdl)
  
  
  rhobed = (wtot - w1000d) / depth # bulk density of fuel bed
  rhobar = ((wtotl * rhodl) + (wtotd * rhodl)) / wtot # particle density of weighted fuel
  betbar = rhobed / rhobar  # packing ratio
  
  # ;if wtotln .gt. 0
  # heating numbers
  hnu1 = w1n * exp(-138 / sg1d)
  hnu10 = w10n * exp( -138 / sg10d)
  hnu100 = w100n * exp( -138 / sg100d)
  
  # conform_dims(dimsizes(wherbn), 0, -1)
  
  # hnherb = where((-500. / sgherb) .lt. -180.218, 0., wherbn * exp(-500. / sgherb))
  if((-500 / sgherb) < -180.218){
    hnherb = 0 # need to check this!
  } else{
    hnherb = wherbn * exp(-500. / sgherb)
  }
  
  
  if((-500 / sgwood) < -180.218){
    hnwood = 0
  } else{
    hnwood = wwoodn * exp(-500 / sgwood)
  } 
  if((hnherb + hnwood) == 0){
    wrat = 0
  } else{
    wrat = (hnu1 + hnu10 + hnu100) / (hnherb + hnwood)
  } 
  
  fmff = ((fm1 * hnu1) + (fm10 * hnu10) + (fm100 * hnu100)) / (hnu1 + hnu10 + hnu100) # fine dead fuel moisture content
  
  if(wtotln > 0){
    extliv = ((2.9 * wrat * (1 - fmff / extmoi) - 0.226) * 100)
    
    # clipping operation
    if(extliv < extmoi) extliv = extmoi
    
  } else{
    extliv = 0
  } 
  
  # weighting factors for rate-of-spread by surface area
  sa1 = (w1dp / rhodl) * sg1d
  sa10 = (w10d / rhodl) * sg10d
  sa100 = (w100d / rhodl) * sg100d
  sherbc = (wherbp / rhodl) * sgherb
  sawood = (wwood / rhodl) * sgwood
  
  # total surface area of dead and live fuel categories by surface area
  sadead = sa1 + sa10 + sa100
  salive = sawood + sherbc
  
  # weighting factors for dead and live fuel classes by surface area
  fct1 = sa1 / sadead
  fct10 = sa10 / sadead
  fct100 = sa100 / sadead
  
  
  if(wtotl > 0){
    fcherb = sherbc / salive
  } else{
    fcherb = 0
  } 
  if(wtotl > 0){
    fcwood = sawood / salive
  } else{
    fcwood = 0
  } 
  
  
  # weighting factors for dead, live fuel categories
  fcded = sadead / (sadead + salive)
  fcliv = salive / (sadead + salive)
  
  # weighted surface area to volume ratios of dead and live fuel categories
  sgbrd = fct1 * sg1d + fct10 * sg10d + fct100 *sg100d
  sgbrl = fcwood * sgwood + fcherb * sgherb
  
  # characteristic surface area to volume ratio
  sgbrt = sgbrd * fcded + sgbrl * fcliv
  
  # weighting factors for dead and live fuel classes by load
  fct1e = w1dp / wtotd
  fct10e = w10d / wtotd
  fct100e = w100d / wtotd
  fct1000e = w1000d / wtotd
  
  if(wtotl > 0){
    fwoode = wwood / wtotl
  } else{
    fwoode = 0
  } 
  if(wtotl > 0){
    fhrbce = wherbp / wtotl
  } else{
    fhrbce = 0
  } 
  
  # weighting factors for dead and live fuel categories by load
  fcdede = wtotd / wtot
  fclive = wtotl / wtot
  wdedne = wtotd * (1 - stdl)
  wlivne = wtotl * (1 - stdl)
  
  sgbrde = (fct1e * sg1d) + (fct10e * sg10d) + (fct100e * sg100d) + (fct1000e * sg1000d)
  sgbrle = (fwoode * sgwood) + (fhrbce * sgherb)
  sgbrte = sgbrde * fcdede + sgbrle * fclive
  betope = 3.348 * (sgbrte^(-0.8189))
  
  gmamxe = (sgbrte^1.5) / (495 + 0.0594 * (sgbrte^1.5)) # weighted max reaction velocity of loading
  ade = 133 * (sgbrte^(-0.7913))
  gmapme = gmamxe * ((betbar / betope)^ade) * exp(ade * (1 - (betbar/ betope))) # weighted optimum reaction velocity of loading (gmaope)
  
  # weighted moisture content of dead, live fuel categories
  wtfmde = fct1e * fm1 + fct10e * fm10 + fct100e * fm100 + fct1000e * fm1000
  wtfmle = fwoode * fmwood + fhrbce * mcherb
  
  dedrte = wtfmde / extmoi
  livrte = wtfmle / extliv
  
  etamde = 1 - 2 * dedrte + 1.5 * (dedrte^2) - 0.5 * (dedrte^3)
  etamde = etamde
  etamde = etamde
  
  # clipping operation
  if(etamde > 1) etamde = 1
  if(etamde < 0) etamde = 0
  
  etamle = 1 - 2 * livrte + 1.5 * (livrte^2) - 0.5 * (livrte^3)
  etamle = etamle
  etamle = etamle
  
  # clipping operation
  if(etamle > 1) etamle = 1
  if(etamle < 0) etamle = 0
  
  ire = fcdede * wdedne * hd * etasdl * etamde # note in MATLAB code there is an hd and hl, but they are equal
  ire = gmapme * (ire + (fclive * wlivne * hd * etasdl * etamle))
  tau = 384. / sgbrt
  
  erc = 0.04 * ire * tau
  
  return(erc)
  
} 
