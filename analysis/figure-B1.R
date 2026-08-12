##########'
#' figure-B1
##'
#' Author: E.A.Melman @RIVM
#' Contributions by:
##'
#' Usage: Run xgb-gapfilling.R first! 
##'
#' Aim: Visualize performance of XGB model
#########' 
# Plot results ----
# -- Plot xy comparison
FigB1a <- ggplot(data = df_xg_fNH3_test, 
                         aes(x = fNH3.flux, y = fNH3.xg)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_abline(linetype = 'dashed') + 
  geom_hex() +
  scale_fill_viridis_c() + 
  geom_label(data = xgb_fNH3_stat %>%
               mutate(x = -0.6, y = 0.35,
                      label = paste0("R2 = ", round(R2, 2))), 
             aes(x = x, 
                 y = y, 
                 label = label), 
             size = 2.5) + 
  # Plot settings
  coord_cartesian(expand = TRUE) + 
  lims(x = c(-1, 0.55), 
       y = c(-1, 0.5)) + 
  labs(tag = '(a)', 
       fill = '#',
       y = expression(paste("Predicted F"["NH"[3]] ~ "("*mu*g~m^-2~s^-1*")")), 
       x = expression(paste("Observed F"["NH"[3]] ~ "("*mu*g~m^-2~s^-1*")"))) + 
  theme(legend.position = 'right', 
        text = element_text(family = "sans")) + 
  facet_wrap(~ Threshold, labeller = labeller(
    Threshold = c("low" = "u['*'] > 0.1", "high" = "u['*'] > 0.3"),
    .default = label_parsed))

# -- Plot diurnal cycle
# Define xticks
tstart <- as.POSIXct(paste0(Sys.Date(), "00:00:00"), tz = "UTC")
tend <- as.POSIXct(paste0(Sys.Date(), "24:00:00"), tz = "UTC")

# Make plot
FigB1b <- ggplot(data = df_xg_fNH3_test %>% 
                      mutate(hr = hour(time.start), 
                             mn = minute(time.start),
                             hhTime = ymd_hms(paste0(today("UTC"), " ", hr, ":", mn,":00"))) %>% 
                      group_by(Threshold, hhTime)  %>% 
                      summarize(across(c(fNH3.flux, fNH3.xg),
                                       list(pp25 = ~ quantile(., probs = 0.25, na.rm = TRUE),
                                            pp50 = ~ quantile(., probs = 0.50, na.rm = TRUE),
                                            pp75 = ~ quantile(., probs = 0.75, na.rm = TRUE), 
                                            ppN = ~ sum(!is.na(.))),
                                       .names = "{col}.{fn}"))) + 
  geom_hline(yintercept = 0) + 
  # Plot observations
  geom_ribbon(aes(x = hhTime,
                  ymin = fNH3.flux.pp25,
                  ymax = fNH3.flux.pp75, 
                  fill = 'Obs.', group = 1),
              alpha = 0.3) +
  geom_line(aes(x = hhTime, y = fNH3.flux.pp50, 
                color = 'Obs.', 
                linewidth = fNH3.flux.ppN, 
                group = 1), 
            lineend = 'round') + 
  scale_linewidth_binned(name = '#', 
                         range = c(0.5, 3), 
                         breaks = c(10, 20, 40)) + 
  # Plot XGB
  geom_ribbon(aes(x = hhTime,
                  ymin = fNH3.xg.pp25,
                  ymax = fNH3.xg.pp75, 
                  fill = 'XGB', group = 1),
              alpha = 0.3) +
  geom_line(aes(x = hhTime, y = fNH3.xg.pp50, color = 'XGB'))  + 
  # Plot settings
  coord_cartesian(expand = FALSE) + 
  scale_y_continuous(
    limits = c(-0.4, 0.1),
    name = expression(paste("F"["NH"[3]] ~ "("*mu*g~m^-2~s^-1*")"))
  ) + 
  scale_color_manual(name = 'Method', 
                     values = c('Obs.' = 'grey25', 
                                'XGB' = RIVM_colors[4])
  ) + 
  scale_fill_manual(name = 'Method', 
                    values = c('Obs.' = 'grey25', 
                               'XGB' = RIVM_colors[4])
  ) + 
  scale_x_datetime(name = NULL, 
                   breaks = seq(from = tstart,
                                to = tend,
                                by = "6 hours"),
                   date_labels = "%H:%M",
                   minor_breaks = "6 hours") + 
  guides(color = guide_legend(title.position = "top"),
         fill = guide_legend(title.position = "top"),
         # linewidth = guide_legend(title.position = "top")
  ) + 
  labs(tag = '(b)') + 
  theme(legend.position = 'right',) + 
  facet_wrap(~ Threshold, labeller = labeller(
    Threshold = c("low" = "u['*'] > 0.1", 
                  "high" = "u['*'] > 0.3"),
    .default = label_parsed))

# -- Built figure 
FigB1 <- FigB1a / FigB1b

# Save figure
ggsave(FigB1,
       file = 'FigB1.pdf', 
       path = fig_path,
       width = wfull, 
       height = wdouble, 
       units = "mm", dpi=600)
ggsave(FigB1,
       file = 'FigB1.png', 
       path = fig_path,
       width = wfull, 
       height = wdouble, 
       units = "mm", dpi=600,
       device=grDevices::png)
