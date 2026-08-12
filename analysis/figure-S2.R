##########' 
#' figure-S2
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage: Run
##' 
#' Aim: Plot windrose of concentration data on polar coordinates
#########' 
# Get temporary data ----
temp_cNH3 <- dat_Loobos_30 %>% 
  # Select relevant variables
  dplyr::select(time.start, season, month, mD.cNH3, BM.WD.382) %>% 
  # Make bins for wind direction
  mutate(
    WD_bins = cut(BM.WD.382, breaks = seq(0, 360, by = 20)),
    month = factor(format(time.start, "%B"),
                   levels = c('January','February', 'March', 'April', 'May', 
                              'June',  'July', 'August', 'September', 'October', 
                              'November', 'December'))) %>% 
  # Group data
  group_by(month, WD_bins) %>%
  # Calculate median cNH3 per hh
  summarize(WD_p50 = quantile(BM.WD.382, .50, na.rm=T),
            cNH3_p25 = quantile(mD.cNH3, 0.25, na.rm = TRUE),
            cNH3_p50 = mean(c(quantile(mD.cNH3, 0.5, na.rm = TRUE),
                              quantile(mD.cNH3, 0.5, na.rm = TRUE)), na.rm=T),
            cNH3_p75 = quantile(mD.cNH3, 0.75, na.rm = TRUE),
            N = sum(!is.na(mD.cNH3))) 

# Add first rdat_Loo_export# Add first row to end to close loop
temp_cNH3 <- rbind(temp_cNH3, temp_cNH3[1, ])

# Add xtick to df
temp_cNH3 <- temp_cNH3 %>% 
  # mutate(WD_value = seq(0,30,20)) %>% 
  mutate(WD_value = as.numeric(gsub("\\(([^,]*),.*", "\\1", levels(WD_bins)[as.numeric(WD_bins)])))

# Make plot in polar coordinates
FigS2 <- ggplot(data = temp_cNH3) +
  # Plot NH3 data
  geom_ribbon(aes(x = WD_value, ymin = cNH3_p25, ymax = cNH3_p75), 
              fill = 'grey25', alpha=0.3) +
  geom_line(aes(x = WD_value, y = cNH3_p50),
            color = 'grey25',lineend = "round") +
  # Plot settings
  lims(y = c(0, 25)) +
  coord_polar(start = 0) +
  labs(x = NULL,
       y = expression(paste("[NH"[3], "]"[mD], " ("*mu*g~m^-3*")")), 
       linewidth = '#') +
  theme(legend.position = 'right', 
        aspect.ratio = 1) + 
  facet_wrap2(~ month, nrow=3, drop=TRUE, 
              strip = strip_themed(
                background_x = elem_list_rect(
                  fill = c("January"  = RIVM_colors[2], 
                           "February" = RIVM_colors[2], 
                           "March"    = RIVM_colors[1],
                           "April"    = RIVM_colors[1], 
                           "May"      = RIVM_colors[1],
                           "June"     = RIVM_colors[1], 
                           "July"     = RIVM_colors[1],
                           "August"   = RIVM_colors[1], 
                           "September"= RIVM_colors[1],
                           "October"  = RIVM_colors[2], 
                           "November" = RIVM_colors[2],
                           "December" = RIVM_colors[2])))) +
  scale_x_continuous(limits = c(0, 360),
                     breaks = seq(0,359,90), 
                     labels = c('N', 'E', 'S', 'W')) + 
  theme(
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_line(colour = "grey90"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(1, 'mm')
  )
FigS2

# Save figure ----
ggsave(FigS2,
       file = 'FigS2.png', 
       path = fig_path,
       width = wfull, 
       height =  1.05*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)
ggsave(FigS2,
       file = 'FigS2.pdf', 
       path = fig_path,
       width = wfull, 
       height =  1.05*wsingle, 
       units = "mm", dpi=600)


