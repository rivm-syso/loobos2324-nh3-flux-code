##########'
#' calc-gamma
##'
#' Author: E.A.Melman @RIVM
#' Contributions by:
##'
#' Usage:
##'
#' Aim: Calculate emission potential from surface temperature and [NH3] at the 
#' leaf surface.
#########'
calcGamma <- function(Tsurf, chi_s){
  gamma_s <- chi_s/((2.75e15/Tsurf)*exp(-1.04e4/Tsurf))
  
  return(gamma_s)
}