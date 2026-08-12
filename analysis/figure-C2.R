##########' 
#' figure-C2
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage:  
##' 
#' Aim: Plot binned observed and DEPAC predicted NH3-flux as a function of [NH3]. 
#########' 
# Get data first ----
temp <- dat_Loobos_30 %>% 
  # rename for convenience
  dplyr::rename(Observed = fNH3.flux, DEPAC = fNH3.DEPAC,
                cNH3 = HT.cNH3) %>% 
  mutate(
    # Filter data
    across(-c(time.start, fNH3.ff2), ~ if_else(
      condition = fNH3.ff2 == TRUE,
      true = .,
      false = NA))) %>%
  # Make longer
  pivot_longer(
    c(Observed, DEPAC),
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

# Set y-limits for all plots ----
y_min <- -1.5
y_max <- 0.1

# Bin fNH3 against cNH3 ----
# -- helper function
summarize_by_bin <- function(data, bin_col, bin_midpoints) {
  data %>%
    group_by(Method, DepPath, !!sym(bin_col)) %>%
    summarize(across(c(flux), list(
      pp50 = ~ median(., na.rm = TRUE),
      pp25 = ~ quantile(., 0.25, na.rm = TRUE),
      pp75 = ~ quantile(., 0.75, na.rm = TRUE),
      ppN = ~ sum(!is.na(.))
    ), .names = "{.col}.{.fn}")) %>%
    pivot_longer(
      cols = contains('flux.'),
      names_to = c("fluxcol", "quantile"),
      names_sep = "\\.p",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = quantile,
      values_from = value
    ) %>%
    drop_na() %>%
    mutate(bin = !!sym(bin_col),
           bin_mid = bin_midpoints[as.numeric(!!sym(bin_col))]) %>%
    group_by(Method, fluxcol, DepPath) %>%
    mutate(p25_clipped = pmin(pmax(p25, y_min), y_max),
           p50_clipped = pmin(pmax(p50, y_min), y_max),
           p75_clipped = pmin(pmax(p75, y_min), y_max)) %>% 
    mutate(Method = as.factor(Method))
}

# -- bin data
result_cNH3 <- summarize_by_bin(
  data = temp, 
  bin_col = "cNH3.bins", 
  bin_midpoints = head(seq(0, 20, 2), -1) + diff(seq(0, 20, 2)) / 2)

# Plor results ----
figC2 <- ggplot(data = result_cNH3) + 
  geom_hline(yintercept = 0) + 
  geom_line( 
    aes(x = bin_mid, y = p50_clipped, 
        color = Method),
    lineend = 'round') + 
  geom_ribbon(aes(x = bin_mid,             
                  ymin = p25_clipped, 
                  ymax = p75_clipped, 
                  fill = Method,
  ), alpha = 0.3) + 
  lims(y = c(y_min, y_max), 
       x = c(0,21)) +
  coord_cartesian(expand = FALSE) + 
  labs(
    x = expression(paste("[NH"[3]*"]"['HT'] ~ "("*mu*g~m^-3*")")), 
    y = expression(paste("F"["NH"[3]] ~ "("*mu*g~m^-2~s^-1*")"))) +
  scale_color_manual(name = NULL, 
                     values = c('Observed' = 'grey25',
                                'DEPAC' = RIVM_colors[2]),
                     labels = c('DEPAC' = expression("DEPAC"['ref'])),
                     breaks =c ('Observed', 'DEPAC')
  ) +
  scale_fill_manual(name = NULL, 
                    values = c('Observed' = 'grey25',
                               'DEPAC' = RIVM_colors[2]),
                    labels = c('DEPAC' = expression("DEPAC"['ref'])),
                    breaks =c ('Observed', 'DEPAC')) +
  facet_wrap(~DepPath,
             labeller = as_labeller(
               c('day_dry' = '(a) Day, dry', 
                 'day_wet' = '(b) Day, wet', 
                 'night_dry' = '(c) Night, dry', 
                 'night_humid' = '(d) Night, humid',
                 'night_wet' = '(e) Night, wet', 
                 'Mixture' = '(f) Mixture')
             ), 
             ncol = 6) + 
  theme(legend.position = 'bottom',
        legend.title.position = 'top', 
        axis.text.x = element_text(hjust = 1))
figC2

# save figure
ggsave(figC2,
       file = 'FigC2.pdf', 
       path = fig_path,
       width = wfull, 
       height = 0.5*wsingle, 
       units = "mm", dpi=600)
ggsave(figC1,
       file = 'FigC2.png', 
       path = fig_path,
       width = wfull, 
       height = 0.5*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)

