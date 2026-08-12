##########'
#' figure-S1
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage: 
##'
#' Aim: Plot overview of meteorological variables during measurement campaign. 
#########' 
# Collect data ----
temp0 <- dat_Loobos_30 %>% 
  mutate(year_month = format(time.start, "%Y-%m"), 
         year_month = as.POSIXct(paste0(year_month, "-15"), format = "%Y-%m-%d", tz='UTC'),
         # Set canopy wetness
         wet.canopy = if_else(
           condition = BM.LW.221 > 0 & BM.LW.200 > 0 & BM.LW.180 > 0, 
           true = 'wet',
           false = 'dry'))

# Get diels --
temp.diel <- temp0 %>%  
  # Select full year
  dplyr::filter(time.start >= as.POSIXct('2023-09-01 00:00:00', tz = 'UTC') & 
                  time.start < as.POSIXct('2024-09-01 00:00:00', tz = 'UTC')) %>% 
  mutate(
    hr = hour(time.start),
    mn = minute(time.start),
    month = factor(format(time.start, "%B"),
                   levels = c('January','February', 'March', 'April', 'May',  
                              'June', 'July', 'August', 'September', 'October', 
                              'November', 'December'))) %>% 
  # Group
  group_by(season, month, hr, mn) %>%
  # Summarize
  summarize(
    RH.avg = mean(BM.RH.382, na.rm = TRUE),
    wet.frac = sum(wet.canopy == 'wet', na.rm=T)/n()) %>% 
  # Make xtick
  mutate(
    hhTime = ymd_hms(paste0(today("UTC"), " ", hr, ":", mn,":00")))

# Get monthly stats --
temp.monthly <- temp0 %>% 
  mutate(Date = as.Date(time.start),
         can.wet.all = BM.LW.221 > 0 & BM.LW.200 > 0 & BM.LW.180 > 0,
         can.wet.day = BM.LW.221 > 0 & BM.LW.200 > 0 & BM.LW.180 > 0 & BM.R.sw.in > 10,
         can.wet.night = BM.LW.221 > 0 & BM.LW.200 > 0 & BM.LW.180 > 0 & BM.R.sw.in < 10) %>% 
  group_by(Date) %>% 
  # Get Tmin and Tmax for each day
  mutate(Tmin = min(BM.Tair.382, na.rm=T), 
         Tmax = max(BM.Tair.382, na.rm=T),
  ) %>% 
  # Get Monthly averages
  ungroup() %>% group_by(year_month) %>% 
  summarize(Tmin.avg = mean(Tmin, na.rm=T),
            Tmax.avg = mean(Tmax, na.rm=T),
            Psum = sum(BM.P, na.rm=T), 
            wet.frac = sum(wet.canopy == 'wet', na.rm=T)/n(),
            wet.frac.day = sum(can.wet.day, na.rm=T)/n(),
            wet.frac.night = sum(can.wet.night, na.rm=T)/n()) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = c(wet.frac.day, wet.frac.night), # can.wet.all, 
    names_to = 'crit',
    values_to = 'wet.frac.daynight'
  )

# Plot monthly results ----
# Common X scale
common_x_scale <- scale_x_datetime(
  name = NULL,
  limits = c(as.POSIXct("2023-08-01", tz = 'UTC'), as.POSIXct("2024-10-01", tz = 'UTC')),
  breaks = seq.POSIXt(from = as.POSIXct("2023-09-01", tz = 'UTC'), 
                      to = as.POSIXct("2024-10-01", tz = 'UTC'), 
                      by = "3 months") %>% with_tz('UTC'),
  date_labels = "%b '%y",
  minor_breaks = "1 months",
  expand = c(0,0)
)

# -- Plots
p_meteo_a <- ggplot(temp.monthly %>% 
                      pivot_longer(c(Tmin.avg, Tmax.avg), 
                                   names_to = 'Temperature', 
                                   values_to = 'Value')) +
  add_missing_cNH3() +
  add_season_rects(28.5, 30) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'), linewidth = 0.25) + 
  geom_line(aes(x = year_month, y = Value, linetype = Temperature)) +
  common_x_scale +
  scale_y_continuous(limits = c(0,30), expand = c(0,0),
                     name = expression('Temperature' ~ '('*degree*C*')')) +
  scale_linetype_manual(
    values = c(1,2),
    labels = c('Tmin.avg' = expression('T'['min']),
               'Tmax.avg' = expression('T'['max']))) + 
  guides(linetype = guide_legend(keyheight = 0.6)) + 
  labs(tag = '(a)', 
       linetype = NULL) +
  theme(legend.position = c(0.42, 0.82),
        legend.background = element_blank())

p_meteo_b <- ggplot(temp.monthly) +
  add_missing_cNH3() +
  add_season_rects(190, 200) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'), linewidth = 0.25) + 
  geom_bar(aes(x = year_month, y = Psum), stat = 'identity') +
  common_x_scale +
  scale_y_continuous(limits = c(0,200), expand = c(0,0),
                     name = 'Precipitation (mm)') +
  labs(tag = '(b)') +
  theme(legend.position = 'none')

p_meteo_c <- ggplot(temp.monthly) +
  add_missing_cNH3() +
  add_season_rects(0.5, 0.525) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'), linewidth = 0.25) + 
  # geom_bar(aes(x = year_month, y = wet.frac), stat = 'identity') +
  geom_col(aes(x = year_month, y = wet.frac.daynight, fill = crit), position = "dodge") +
  common_x_scale +
  scale_y_continuous(limits = c(0,0.525), expand = c(0,0),
                     name = 'Fraction of observations \n with canopy wetness (-)') +
  scale_fill_manual(
    name = NULL,
    values = c(
      'wet.frac.day' = RIVM_colors[8],
      'wet.frac.night' = RIVM_colors[7]),
    labels = c('wet.frac.day' = expression('Daytime'),
               'wet.frac.night' = expression('Nighttime'))) +
  guides(fill = guide_legend(keywidth = 0.6, keyheight = 0.6)) + 
  labs(tag = '(c)') +
  theme(legend.position = c(0.12, 0.82),
        legend.background = element_blank())

# Plot diel results ----
# -- Define xticks range
tstart <- as.POSIXct(paste0(Sys.Date(), "00:00:00"), tz = "UTC")
tend <- as.POSIXct(paste0(Sys.Date(), "24:00:00"), tz = "UTC")

# -- plot RH
p_meteo_d <- ggplot(temp.diel) + 
  # plot relative humidity
  geom_line(aes(x = hhTime, y = RH.avg, 
                color = month, 
                linetype = season)) + 
  # Plot properties
  scale_x_datetime(
    name = 'Time of day',
    expand = c(0.0, 0.0),
    breaks = seq(from = tstart,
                 to = tend,
                 by = "6 hours"),
    date_labels = "%H",
    minor_breaks = "6 hours") +
  scale_y_continuous(
    limits = c(50,100),
    expand = c(0.0, 0.0),
    name = 'Relative humidity (%)'
  ) + 
  scale_color_manual(
    name = 'Month', 
    values = RIVM_colors) +
  labs(tag = '(e)') +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(ncol = 1)) + 
  theme(#legend.background = element_blank(),
    legend.title.position = 'top')

# -- plot wet.frac
p_meteo_e <- ggplot(temp.diel) + 
  # plot relative humidity
  geom_line(aes(x = hhTime, y = wet.frac, 
                color = month, 
                linetype = season)) + 
  # Plot properties
  scale_x_datetime(
    name = 'Time of day',
    expand = c(0.0, 0.0),
    breaks = seq(from = tstart,
                 to = tend,
                 by = "6 hours"),
    date_labels = "%H",
    minor_breaks = "6 hours") +
  scale_y_continuous(
    limits = c(0,1),
    expand = c(0.0, 0.0),
    name = 'Fraction of observations \n with canopy wetness (-)'
  ) + 
  scale_color_manual(
    name = NULL, 
    values = RIVM_colors) +
  labs(tag = '(d)') +
  guides(color = guide_legend(nrow = 2),
         linetype = guide_legend(ncol = 1)) + 
  theme(#legend.background = element_blank(),
    legend.title.position = 'top')

# build figure ----
# -- Get legend for panels d and e
legend <- get_legend(p_meteo_d + theme(legend.position = "right",
                                       legend.box = "horizontal"
))

# -- Make figure
FigS1 <- plot_grid(
  # Monthly plots
  plot_grid(p_meteo_a,
            p_meteo_b,
            p_meteo_c,
            align = 'v',
            axis = 'lr',
            ncol = 1),
  # Diurnal plots,
  plot_grid(
    plot_grid(
      p_meteo_e + theme(legend.position = "none"),
      p_meteo_d + theme(legend.position = "none"),
      align = 'h',
      axis = 'tb',
      ncol = 2),
    legend,
    ncol = 1,
    rel_heights = c(1,0.4)),
  ncol = 1,
  align = 'v',
  axis = 'lr',
  rel_heights = c(2,1))

# -- Save figure
ggsave(FigS1,
       path = fig_path,
       file = 'FigS1.pdf', 
       width = wfull, 
       height = 2.1*wsingle, 
       units = "mm", dpi=600)
ggsave(FigS1,
       path = fig_path,
       file = 'FigS1.png', 
       width = wfull, 
       height = 2.1*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)

