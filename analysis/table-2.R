##########' 
#' table-2
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage:  
##' 
#' Aim: calculate exchange efficiency (see aslo figure-7)
#########' 
# Get data first ----
temp <- dat_Loobos_30 %>% 
  # rename for convenience
  dplyr::rename( Observed = fNH3.flux, u = BM.WS.382, cNH3 = HT.cNH3) %>% 
  mutate(
    # Filter data
    across(-c(time.start, fNH3.ff2), ~ if_else(
      condition = fNH3.ff2 == TRUE,
      true = .,
      false = NA)),
    # Calculate ra and rb
    ra = calcRa(u, u.star),
    rb = calcRb(u, u.star),
    # Calculate max exchange velocity
    fNH3.max = -cNH3*(1/(ra + rb))
  ) %>%
  # Make longer
  pivot_longer(
    c(Observed, fNH3.max),
    values_to = 'flux',
    names_to = 'Method'
  ) %>%
  # Make conditions for rw or rs
  mutate(
    DepPath = case_when(
      BM.LW.221 == 0 & BM.LW.200 == 0 & BM.LW.180 == 0 & BM.RH.382 < 60 & BM.R.sw.in > 10 ~ 'day_dry',
      BM.LW.221 == 0 & BM.LW.200 == 0 & BM.LW.180 == 0 & BM.RH.382 > 60 & BM.R.sw.in < 10 ~ 'night_humid',
      BM.RH.382 < 60 & BM.R.sw.in < 10 & BM.LW.221 == 0 & BM.LW.200 == 0 & BM.LW.180 == 0 ~ 'night_dry',
      BM.LW.221 > 0 & BM.LW.200 > 0 & BM.LW.180 > 0 & BM.R.sw.in < 10 ~ 'night_wet',
      BM.LW.221 > 0 & BM.LW.200 > 0 & BM.LW.180 > 0 & BM.R.sw.in > 10 ~ 'day_wet',
      TRUE ~ 'Mixture'
    ),
    DepPath = factor(DepPath, 
                     levels = c('day_dry', 'day_wet', 'night_dry', 
                                'night_humid', 'night_wet', 'Mixture'))
  ) %>% 
  # Make bins
  mutate(cNH3.bins = cut(cNH3, breaks = seq(0,20,2))) 

# Calculate average efficiency and Count samples
temp %>%
  group_by(DepPath) %>%
  pivot_wider(values_from = 'flux', names_from = 'Method') %>%
  summarize(
    efficiency.avg = mean(Observed / fNH3.max, na.rm = TRUE),
    efficiency.std = sd(Observed / fNH3.max, na.rm = TRUE),
    N = sum(!is.na(Observed)) / 18049 * 100
  ) %>%
  arrange(desc(efficiency.avg)) %>% 
  knitr::kable(
    digits = 2, format = "simple", 
    caption = "Summary of Efficiency by Deposition Path",
    booktabs = TRUE
  )
