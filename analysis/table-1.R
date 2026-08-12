##########' 
#' table-1
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage: 
##' 
#' Aim: acceptance rate to filtering conditions.
#' *NOTE*: this code shows acceptance rate after application of filters #1-3
#########'
# Set campaign and HT duration ----
# -- Calculate number of 30-minute intervals for each period
# HT Deployment in 2023
intervals_ht23 <- as.numeric(difftime(ht_deployment23_end, 
                                      ht_deployment23_start, 
                                      units = "mins")) / 30

# Intermediate period (between deployments)
intervals_intermediate <- as.numeric(difftime(ht_deployment24_start, 
                                              ht_deployment23_end, 
                                              units = "mins")) / 30 + 1

# HT Deployment in 2024
intervals_ht24 <- as.numeric(difftime(ht_deployment24_end, 
                                      ht_deployment24_start, 
                                      units = "mins")) / 30

# -- Total intervals with HT deployment and campaign length
n_HT <- intervals_ht23 + intervals_ht24
n_campaign <- n_HT + intervals_intermediate

# Make table ----
dat_Loobos_30 %>% 
  summarize(
    filter4_campaign = sum(fNH3.ff1)/n_campaign*100,
    filter4_HT  = sum(fNH3.ff1)/n_HT*100,
    filter5_campaign = sum(fNH3.ff2)/n_campaign*100,
    filter5_HT = sum(fNH3.ff2)/n_HT*100,
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Filter #", "Acceptance rate"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = `Acceptance rate`,
    values_from = value
  ) %>% 
  knitr::kable(
    digits = 1, format = "simple", 
    caption = "Acceptance rate (%) after filter application for total campaign 
    duration and HT deployment only.",
    booktabs = TRUE
  )

