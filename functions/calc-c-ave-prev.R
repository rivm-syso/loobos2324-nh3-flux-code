##########'
#' calc-c-ave-prev.R
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage: 
##'
#' Aim: Calculate running mean concentration of previous period (ndays) 
#########' 
calc_c_ave_prev = function(cNH3, cSO2, ndays = 31, interval = 1800) {
  # Calculate window size based on seconds in a month
  window_size_base <- round(ndays*24*3600 / interval)
  
  # Pre-allocate c_ave_prev
  c_ave_prev_nh3 <- rep(NA, length(cNH3))
  c_ave_prev_so2 <- rep(NA, length(cSO2))
  
  for (i in 1:length(cNH3)) {
    
    # For the first month, window size is equal to number of records up to that moment
    window_size <- min(i, window_size_base)
    
    if (i > 1 & is.na(cNH3[i])) {
      cNH3_proxy <- c_ave_prev_nh3[i - 1]
      cSO2_proxy <- c_ave_prev_so2[i - 1]
    } else {
      cNH3_proxy <- cNH3[i]
      cSO2_proxy <- cSO2[i]
    }
    c_ave_prev_nh3[i] <- sum(c((window_size - 1)*c_ave_prev_nh3[i-1], cNH3_proxy), na.rm=TRUE) / window_size
    c_ave_prev_so2[i] <- sum(c((window_size - 1)*c_ave_prev_so2[i-1], cSO2_proxy), na.rm=TRUE) / window_size
  }
  
  # Return output
  return(c_ave_prev_nh3)
}