##########'
#' figure-6
##'
#' Author: E.A.Melman @RIVM
#' Contributions by:
##'
#' Usage: 
##'
#' Aim: Plot diurnal evolution of fNH3 for each month during the measurement 
#' campaign. 
#########'
# Make temporary data structure ----
# -- Get data
temp_fNH3 <- dat_Loobos_30 %>% 
  # Select relevant variables
  select(time.start, season, month, Observed = fNH3.flux, fNH3.ff2, Rg = BM.R.sw.in) %>% 
  # Filter data
  subset(fNH3.ff2) %>%  
  # Get time and months
  mutate(
    hr = hour(time.start),
    mn = minute(time.start),
    month = factor(format(time.start, "%B"),
                   levels = c('February', 'March', 'April', 'May', 'June',
                              'July', 'August', 'September', 'October', 
                              'November'))) 

# ---- Define xticks
tstart <- as.POSIXct(paste0(Sys.Date(), "00:00:00"))
tend <- as.POSIXct(paste0(Sys.Date(), "24:00:00"))

# -- Calculate diurnal cycle
# Clip data limits
y_min <- -0.4
y_max <- 0.1

# Calculate monthly diurnals
temp_dc <- temp_fNH3 %>% 
  # Group variables
  group_by(season, month, hr, mn) %>% 
  # Summarize data
  summarize(across(c(Observed, Rg), list(
    p50 = ~ median(., na.rm = TRUE),
    p25 = ~ quantile(., 0.25, na.rm = TRUE),
    p75 = ~ quantile(., 0.75, na.rm = TRUE),
    n = ~ sum(!is.na(.))
  ), .names = "{.col}.{.fn}")) %>%
  # Make timeformat for plotting
  mutate(hhTime = as.POSIXct(paste(hr, mn, "00", sep = ":"),
                             format = "%H:%M:%S")) %>%
  # Clip data
  mutate(p25_clipped = pmin(pmax(Observed.p25, y_min), y_max),
         p75_clipped = pmin(pmax(Observed.p75, y_min), y_max))

# -- Caculate means, medians, daytime, nighttime
temp_stat <- temp_dc %>% 
  # Get classification
  mutate(day.night = if_else(
    condition = Rg.p50 < 10, 
    true = 'Night',
    false = 'Day'
  )) %>% 
  # Group data
  group_by(month, day.night) %>% 
  # Get stats
  summarize(across(c(Observed.p50), list(
    p50 = ~ median(., na.rm = TRUE),
    p25 = ~ quantile(., 0.25, na.rm = TRUE),
    p75 = ~ quantile(., 0.75, na.rm = TRUE),
    n = ~ sum(!is.na(.))
  ), .names = "{.col}.{.fn}")) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# For day segment
day_segment <- temp_stat %>% 
  dplyr::filter(day.night == "Day") %>%
  mutate(
    x = tend - hours(3) - minutes(30),
    xend = tend - minutes(30),
    y = Observed.p50.p50,
    yend = Observed.p50.p50
  )

# For night segment
night_segment <- temp_stat %>% 
  dplyr::filter(day.night == "Night") %>%
  mutate(
    x = tstart,
    xend = tstart + hours(3),
    y = Observed.p50.p50,
    yend = Observed.p50.p50
  )

# Plot data ----
# ---- Flux plot
fig6 <- ggplot(data = temp_dc) + 
  geom_abline(slope = 0, intercept = 0, color = 'black') +
  # =--- Plot data
  geom_ribbon(aes(x = hhTime,
                  ymin = p25_clipped,
                  ymax = p75_clipped),
              alpha=0.3) +
  geom_line(aes(x = hhTime, 
                y = Observed.p50, 
                linewidth = Observed.n),
            lineend = "round") +
  # --- Median segments
  geom_segment(data = day_segment,
               aes(x = x, xend = xend, y = y, yend = yend),
               color = RIVM_colors[4],
               size = 0.5) +
  geom_segment(data = night_segment,
               aes(x = x, xend = xend, y = y, yend = yend),
               color = RIVM_colors[4],
               size = 0.5) + 
  # ---- Plot properties
  scale_linewidth_binned(
    name = '#', 
    range = c(0.5, 2), 
    breaks = c(0, 5, 10, 20)) + 
  scale_color_manual(
    values = c("Observed" = 'black')) +
  scale_fill_manual(
    values = c("Observed" = 'black')) +
  scale_x_datetime(
    name = 'Hour of day',
    breaks = seq(from = tstart,
                 to = tend - minutes(30),
                 by = "6 hours"),
    date_labels = "%H",
    minor_breaks = "6 hours") +
  scale_y_continuous(
    limits = c(-0.4, 0.1),
    labels = scales::number_format(accuracy = 0.1),
    name = expression(paste("F"["NH"[3]] ~ "("*mu*g~m^-2~s^-1*")")),
    sec.axis = sec_axis(~ ., 
                        labels = scales::number_format(accuracy = 0.1),
                        name = NULL),
  ) + 
  coord_cartesian(expand = FALSE) +
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
        legend.title.position = 'top',
        panel.spacing.y = unit(1, 'mm'),
        panel.spacing.x = unit(1, 'mm'),) 
fig6

# ---- Save figures
ggsave(fig6,
       file = 'Fig6.png', 
       path = fig_path,
       width = wfull, 
       height =  0.85*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)
ggsave(fig6,
       file = 'Fig6.pdf', 
       path = fig_path,
       width = wfull, 
       height =  0.85*wsingle, 
       units = "mm", dpi=600)

