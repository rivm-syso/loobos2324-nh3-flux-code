##########'
#' annual-nh3-deposition-load
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage:
##'
#' Aim: Calculate annual deposition load
#########' 
# calculate annual load ----
dat_Loobos_30 %>% 
  dplyr::filter(
    time.start >= as.POSIXct('2023-09-01 00:00:00', tz = 'UTC') &
      time.start < as.POSIXct('2024-09-01 00:00:00', tz = 'UTC')
  ) %>%
  mutate(
    fNH3.flux = if_else(
      condition = fNH3.ff1 & !is.na(fNH3.DEPAC), 
      true = fNH3.flux, 
      false = NA),
    fNH3.DEPAC = if_else(
      condition = fNH3.ff1 & !is.na(fNH3.DEPAC), 
      true = fNH3.DEPAC, 
      false = NA),
  ) %>% 
  summarize(
    Observed = sum(fNH3.flux*
                     conv_sec_to_halfhour*
                     conv_ugNH3m2_to_kgNha, 
                   na.rm=T), 
    `DepAC-predicted` = sum(fNH3.DEPAC*
                         conv_sec_to_halfhour*
                         conv_ugNH3m2_to_kgNha, 
                    na.rm=T), 
    `Gap-filled` = sum(fNH3.flux.gf*
                              conv_sec_to_halfhour*
                              conv_ugNH3m2_to_kgNha)) %>% 
  knitr::kable(
    digits = 1, format = "simple", 
    caption = "Annual NH3 deposition load (kg N ha-1 yr-1) with (i) only 
    observations, (ii) DepAC predicted fluxes and (iii) gap-filled fluxes",
    booktabs = TRUE
  )



