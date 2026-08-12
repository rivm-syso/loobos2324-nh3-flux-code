##########' 
#' figure-C1
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage: 
##' 
#' Aim: Plot median diurnal cycle of observed and DEPAC predicted NH3-flux.
#########' 
# Collect data ----
temp0 <- dat_Loobos_30 %>% 
  # Select relevant variables
  dplyr::select(
    time.start, fNH3.ff2,
    Observed = fNH3.flux, DEPAC = fNH3.DEPAC, 
  ) %>%
  rowwise() %>%
  # Filter data
  mutate(
    across(c(Observed, DEPAC), 
           ~ if_else(
             condition = fNH3.ff2 == TRUE & all(!is.na(c(Observed, DEPAC))), 
             true = ., 
             false = NA_real_)), 
    month = factor(format(time.start, "%B"),
                   levels = c('February', 'March', 'April', 'May', 'June', 
                              'July', 'August', 'September', 'October', 
                              'November')))

# Calculate  monthly median diurnal cycle ----
# -- Define clips
y_min <- -0.4
y_max <- 0.1

# -- Calculate dc's
temp_dc <- temp0 %>% 
  mutate(
    hr = hour(time.start),
    mn = minute(time.start)
  ) %>% 
  group_by(month, hr, mn) %>% 
  summarize(across(c(Observed, DEPAC),
                   list(pp25 = ~ quantile(., probs = 0.25, na.rm = TRUE),
                        pp50 = ~ quantile(., probs = 0.50, na.rm = TRUE),
                        pp75 = ~ quantile(., probs = 0.75, na.rm = TRUE), 
                        ppN = ~ sum(!is.na(.))),
                   .names = "{col}.{fn}")) %>% 
  # Define hhTime for xticks
  mutate(hhTime = ymd_hms(paste0(today("UTC"), " ", hr, ":", mn,":00"))) %>%
  pivot_longer(
    cols = starts_with(c("Observed", "DEPAC")),
    names_to = c("fluxcol", "quantile"),
    names_sep = "\\.p",
    values_to = "value") %>%
  pivot_wider(
    names_from = quantile,
    values_from = value) %>% 
  drop_na() %>%
  # Clip data
  mutate(p25_clipped = pmin(pmax(p25, y_min), y_max),
         p75_clipped = pmin(pmax(p75, y_min), y_max))

# Plot results ----
# -- Define xticks range
tstart <- as.POSIXct(paste0(Sys.Date(), "00:00:00"), tz = "UTC")
tend <- as.POSIXct(paste0(Sys.Date(), "24:00:00"), tz = "UTC")

# -- Make plot
figC1 <- ggplot(data = temp_dc) + 
  geom_hline(yintercept = 0) +
  # ---- p25 - p75 uncertainty range
  geom_ribbon(aes(hhTime,             
                  ymin = p25_clipped, 
                  ymax = p75_clipped, 
                  fill = fluxcol), alpha = 0.3) + 
  # # ---- Mid value (p50)
  geom_line(data = subset(temp_dc, fluxcol == 'fNH3.flux'),
            aes(x = hhTime,
                y = p50,
                linewidth = pN,
                colour=fluxcol), 
            lineend = 'round') +
  geom_line(data = subset(temp_dc, fluxcol != 'fNH3.flux'),
            aes(x = hhTime,
                y = p50,
                colour=fluxcol)) +
  # ---- Plot properties
  lims(y = c(y_min, y_max)) + 
  scale_linewidth_binned(name = '#', 
                         range = c(0.5, 2), 
                         breaks = c(0, 5, 10, 20)) + 
  scale_color_manual(
    name = 'Flux', 
    values = c('grey25', RIVM_colors[2]),
    breaks = c('Observed', 'DEPAC'),
    labels = c('DEPAC' = expression('DEPAC'['ref']))
  ) +
  scale_fill_manual(
    name = 'Flux', 
    values = c('grey25', RIVM_colors[2]),
    breaks = c('Observed', 'DEPAC'),
    labels = c('DEPAC' = expression('DEPAC'['ref']))
  ) +
  scale_x_datetime(
    breaks = seq(from = tstart,
                 to = tend,
                 by = "6 hours"),
    date_labels = "%H",
    minor_breaks = "6 hours"
  ) +
  coord_cartesian(expand = FALSE) +
  labs(
    x = 'Hour of day', 
    y = expression(paste("F"["NH"[3]] ~ "("*mu*g~m^-2~s^-1*")"))) + 
  facet_wrap2(~ month, nrow=2, drop=TRUE, 
              strip = strip_themed(
                background_x = elem_list_rect(
                  fill = c("February" = RIVM_colors[2], 
                           "March"    = RIVM_colors[1],
                           "April"    = RIVM_colors[1], 
                           "May"      = RIVM_colors[1],
                           "June"     = RIVM_colors[1], 
                           "July"     = RIVM_colors[1],
                           "August"   = RIVM_colors[1], 
                           "September"= RIVM_colors[1],
                           "October"  = RIVM_colors[2], 
                           "November" = RIVM_colors[2])))) +
  theme(legend.position = 'right',
        legend.title.position = 'top')
figC1

# -- Save figure
ggsave(figC1,
       file = 'FigC1.png', 
       path = fig_path,
       width = wfull, 
       height =  0.6*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)
ggsave(figC2,
       file = 'FigC1.pdf', 
       path = fig_path,
       width = wfull, 
       height =  0.6*wsingle, 
       units = "mm", dpi=600)
