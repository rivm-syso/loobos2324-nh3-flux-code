##########'
#' table-3
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage:
##'
#' Aim: Calculate some annual statistics for Loobos campaign
#' *NOTE*: This code contains calculations on the Loobos dataset only. 
#########'
# Collect data ----
dat_Loobos_30 %>% 
  dplyr::select(time.start, LAI, ell, v.tke, mD.cNH3, BM.P, BM.Tair.382, 
                BM.LW.180, BM.LW.200, BM.LW.221) %>% 
  # Summarize over campaign lengths
  summarize(
    # Report LAI range
    LAI.min = min(LAI, na.rm = TRUE),
    LAI.max = max(LAI, na.rm = TRUE),
    # Report turbulent statistics
    ell.avg = mean(ell, na.rm = TRUE),
    ell.sd = sd(ell, na.rm = TRUE),
    v.tke.avg = mean(v.tke, na.rm = TRUE),
    v.tke.sd = sd(v.tke, na.rm = TRUE), 
    # Report NH3 concentration
    NH3.median = median(mD.cNH3, na.rm = TRUE), 
    # Report meteo statistics
    Tair.avg = mean(BM.Tair.382, na.rm = TRUE),
    P.sum = sum(BM.P, na.rm=T),
    wet_can.frac = sum(BM.LW.180  > 0 & 
                         BM.LW.200 > 0 & 
                         BM.LW.221 > 0, na.rm = T)/17568*100, 
    ) %>%
  pivot_longer(everything(), 
               names_to = "Variable", 
               values_to = "Value") %>% 
  # Add Annual statistics
  rbind(dat_Loobos_30 %>% 
          # Select full year (1-sep-2023 till 31-aug-2024)
          dplyr::filter(
            time.start >= as.POSIXct('2023-09-01 00:00:00', tz = 'UTC') &
              time.start < as.POSIXct('2024-09-01 00:00:00', tz = 'UTC')
          ) %>%
          summarize(
            # Report cumulative fluxes
            fNH3.sum = sum(fNH3.flux.gf*conv_sec_to_halfhour*conv_ugNH3m2_to_kgNha),
            fNH3.sd = 0.14*fNH3.sum,
            NEE.sum = sum(NEE.gf*conv_sec_to_halfhour*conv_umolCO2m2_to_tonCha),
            GPP.sum = sum(GPP.gf*conv_sec_to_halfhour*conv_umolCO2m2_to_tonCha)) %>%
          pivot_longer(everything(), 
                       names_to = "Variable", 
                       values_to = "Value")) %>% 
  knitr::kable(
    digits = 1, format = "simple", 
    caption = "Some relevant statistics for Loobos. Variable name extensions 
    refer to type of data summary: minimum (min), maximum (max), avg (average), 
    sd (standard deviation), fraction (frac), median and sum.",
    booktabs = TRUE
  )
  