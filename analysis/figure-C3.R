##########'
#' figure-C3
##'
#' Author: E.A.Melman @RIVM
#' Contributions by:
##'
#' Usage:
##'
#' Aim: Calculate stomatal emission potential and compare against values 
#' reported in literature.
#' *NOTE*: The plot only contains the derived Gamma value for Loobos. Values from 
#' literature can be downloaded from Melman et al. (2024). That data was 
#' originally published by Wichink Kruit et al. (2010). Please  cite their  
#' work when using this data.
#'
#' References:
#' Melman, E., Rutledge-Jonker, S., Frumau, A., Hensen, A., van Pul, A., Stolk, 
#'   A., Wichink Kruit, R., & van Zanten, M. (2024). Data and code for 
#'   "Measurements and model results of a two-year dataset of ammonia exchange 
#'   over a coniferous forest in the Netherlands" [Data set]. In Atmospheric  
#'  Environment (Version v1). National Institute for Public Health and the  
#'  Environment. https://doi.org/10.21945/17e65dbc-789d-44d7-b766-425475e30711.
#' Wichink Kruit, R., van Pul, W., Sauter, F., van den Broek, M., Nemitz, E., 
#'   Sutton, M., Krol, M., and Holtslag, A. (2010). Modeling the surface– 
#'   atmosphere exchange of ammonia, Atmospheric Environment, 44, 945–957, 
#'   https://doi.org/10.1016/j.atmosenv.2009.11.049.
#########'
# Collect data ----
temp0 <- dat_Loobos_30 %>% 
  # Approximate surface temperature with Stefan Boltzmann
  mutate(BM.Tsurf = (BM.R.lw.out/(5.67 * 10^-8))^0.25) %>% 
  # Select relevant variables. 
  dplyr::select(time.start, fNH3.flux, HT.cNH3, fNH3.ff2, zoL, BM.R.sw.in, 
         BM.RH.382, starts_with("BM.LW"), BM.Tsurf) %>% 
  mutate(
    # Filter data
    fNH3.flux = if_else(
      condition = fNH3.ff2, 
      true = fNH3.flux, 
      false = NA),
    # Caculate emission potential gamma_s
    gamma.s = calcGamma(BM.Tsurf, HT.cNH3), 
  )

# Derive stomatal emission potential ----
# -- Filter data for conditions where x_a == x_c and stom is dominant path 
# Define filters
cond.fNH3 <- temp0$fNH3.ff2 == 1
cond.day <- temp0$BM.R.sw.in > 10
cond.RH <- temp0$BM.RH.382 < 60
cond.LW <- temp0$BM.LW.180 == 0 & temp0$BM.LW.200 == 0 & temp0$BM.LW.221 == 0 &
  temp0$BM.LW.157 == 0 & temp0$BM.LW.112 == 0 & temp0$BM.LW.074 == 0 &
  temp0$BM.LW.045 == 0 & temp0$BM.LW.024 == 0
cond.zoL <- temp0$zoL < 0.1
cond.fzero <- abs(temp0$fNH3.flux) < 0.02

# Collect filters
cond.chi_c <- cond.fNH3 & cond.day & cond.zoL & 
  cond.LW & cond.RH & cond.fzero   

# -- Apply filters and calculate gamma_s
gamma_s <- temp0 %>% 
  mutate(
    gamma.s =  if_else(
      condition = cond.chi_c, 
      true = gamma.s, 
      false = NA)) %>%
  summarise(
    across(c(HT.cNH3, gamma.s), 
           list(median = ~ median(., na.rm=T), 
                N = ~ sum(!is.na(.)) 
                # std = ~ sd(., na.rm=T)
                ), 
           .names = "{.col}_{.fn}")
  )

# -- Show values
gamma_s %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>% 
  knitr::kable(
    digits = 0, format = "simple", 
    caption = "Derived Gamma_s value for Loobos",
    booktabs = TRUE
  )

# Plot gamma_s against [NH3]  ----
figC3 <- ggplot(data = gamma_s) + 
  # ---- Add linear fit
  geom_abline(slope = 360, intercept = 0) +
  geom_text(x = 8.5, y = 4000, 
            label = expression(Gamma == 360 %*% "[" * NH[3] * "]"), 
            size = 2.5,
            parse = TRUE) +
  # ---- Add points
  geom_point(aes(x = HT.cNH3_median, y = gamma.s_median, color = "Loobos")) +
  # ---- Plot settigns
  labs(x = expression(paste("'long-term' [NH"[3], "]" ~ "("*mu*g~m^-3*")")),
       y = expression(Gamma[s] ~ "[-]"), 
       label = NULL) + 
  lims(y = c(0,5000),
       x = c(0,10)) + 
  scale_color_manual(values = c('Loobos' = 'grey25'), 
                     breaks = c('Loobos')) + 
  theme(legend.position = 'none', 
        legend.title = element_blank(),
        legend.background = element_blank())
figC3

# -- Save figure 
ggsave(figC3,
       file = 'FigC3.pdf', 
       path = fig_path,
       width = wsingle, 
       height = 0.6*wsingle, 
       units = "mm", dpi=600)
ggsave(figC3,
       file = 'FigC3.png', 
       path = fig_path,
       width = wsingle, 
       height = 0.6*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)

