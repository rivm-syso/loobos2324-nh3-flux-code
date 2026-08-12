##########' 
#' meteo-functions
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage:  
##' 
#' Aim: define functions to calculate 
#'   (i) surface temperature (Ts) from outgoing longwave radiation, 
#'   (ii) vapor pressure deficit
#'   (iii) atmospheric resistances ra and rb
#########' 
# (i) Calc_Ts ----
# Function to calculate surface temperature 
calcTs <- function(LWout){
  const.SB <- 
    Tsurf <- (LWout/const.SB)^0.25
  
  return(Tsurf)}

# (ii) Calc_VPD ----
# Function to calculate saturation vapor pressure (es) in kPa using temperature in Kelvin
calces <- function(T_kelvin) {
  return(0.61078 * exp((17.269 * (T_kelvin - 273.15)) / ((T_kelvin - 273.15) + 237.3)))
}

calcVPD <- function(T_kelvin, RH) {
  # Calculate saturation vapor pressure (es) at temperature T_kelvin
  es <- calces(T_kelvin)
  
  # Calculate actual vapor pressure (ea) in kPa
  ea <- (RH / 100) * es
  
  # Calculate vapor pressure deficit (VPD) in kPa
  VPD <- es*(1 - RH/100)
  
  return(VPD)
}

# (iii) calculate atmopsheric resistances ra and rb ----
# Get ra
calcRa <- function(u, u.star) {
  ifelse(
    test = u > 0 & u.star > 0.01,
    yes = u / (u.star^2),
    no = 500
  )
}

# Get rb
calcRb <- function(u, u.star) {
  # Set constant
  thk    <- 0.20e-4 #' thermal diffusivity of dry air 20 C
  diffc  <- 0.21e-4 #' diffusivity of NH3
  
  ifelse(
    test = u > 0 & u.star > 0.01,
    yes = 5 / u.star * (thk / diffc)^(2/3),
    no = 100
  )
}
