##########'
#' figure-3
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage:
##'
#' Aim: Plot timeseries of LAI, GCC, GPP and NEE
######### '
# Get data ----
# -- Calculate daily carbon uptake
temp_NEE <- dat_Loobos_30 %>% 
  mutate(date = as.Date(time.start)) %>% 
  group_by(date) %>% 
  mutate(NEE.daily = sum(NEE.gf), 
         GCC.daily = mean(GCC), 
         LAI.daily = mean(LAI.gf)) %>% 
  ungroup() %>% 
  dplyr::select(time.start, NEE.daily, GCC.daily, LAI.daily)

# -- veff data
temp_monthly <- dat_Loobos_30 %>%
  # Select full year
  dplyr::filter(time.start >= as.POSIXct('2023-09-01 00:00:00', tz = 'UTC') & 
                  time.start < as.POSIXct('2024-09-01 00:00:00', tz = 'UTC')) %>% 
  # Get grouping variables
  mutate(Site = 'Loobos',
         year_month = format(time.start, "%Y-%m")) %>% 
  # Calculate daily values
  group_by(Site, year_month) %>% 
  # Get monthly values
  summarize(
    GPP = sum(-GPP.gf*conv_sec_to_halfhour*conv_umolCO2m2_to_tonCha,0, na.rm=TRUE)) %>% 
  ungroup()

# Map yday to climatology
dat_plot <- data.frame(time.start = seq.POSIXt(from = start_time, 
                                               to = end_time, 
                                               by = '1 day',
                                               tz = "UTC")) %>% 
  # Add carbon uptake
  left_join(temp_NEE) %>% 
  mutate(
    # Calculate rolling mean
    NEE.daily.rm = rollmean(NEE.daily, k = 14, fill = NA, align = 'center')
  )

# Plot yearly cycle
p_a <- ggplot(data = dat_plot) + 
  # -- annotate seasons and year change
  add_missing_fNH3() +
  add_season_rects(3.3, 3.465) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'), linewidth = 0.25) + 
  # -- Start leaf growth
  geom_vline(xintercept = as.POSIXct('24-05-2024', format='%d-%m-%Y', tz = "UTC"), linetype = 3) +
  # -- Plot LAI and NEE
  geom_line(aes(x = time.start,
                y = LAI.daily,
                color = 'LAI'
  ), 
  ) +
  geom_point(data = dat_Loobos_30 %>%
               dplyr::filter(time.start != as.POSIXct('24-05-2024', format='%d-%m-%Y', tz = "UTC")),
             aes(x = time.start,
                 y = LAI,
                 color = 'LAI')) +
  # -- Plot GCC
  geom_line(aes(x = time.start,
                y = GCC.daily*100-39,
                color = 'GCC')) +
  # -- Plot GPP
  geom_line(data = temp_monthly %>%
              mutate(time.mid = as.POSIXct(paste0(year_month, '-15'), format = '%Y-%m-%d')),
            aes(x = time.mid,
                y = -GPP,
                color = 'GPP')) +
  geom_point(data = temp_monthly %>%
               mutate(time.mid = as.POSIXct(paste0(year_month, '-15'), format = '%Y-%m-%d')),
             aes(x = time.mid,
                 y = -GPP,
                 color = 'GPP'), 
             shape = 4) +
  # Plot layout
  geom_text(
    data = data.frame(
      time.start = min(dat_plot$time.start, na.rm = TRUE),
      y = 3.2
    ),
    aes(x = time.start, y = y),
    label = "(a)",
    hjust = -0.1, vjust = 0.5,
    size = 3.5
  ) + 
  scale_color_manual(
    name = NULL,
    values = c(
      'LAI' = 'black',
      'GPP' = RIVM_colors[4],
      'GCC' = RIVM_colors[9])) +
  guides(color = guide_legend(ncol = 3)) + 
  scale_y_continuous(
    limits = c(0,3.465),
    name = expression(atop('-GPP (ton C ha'^{-1}~'month'^{-1}*')',"LAI (" * m^{2} * m^{-2} *")")),
    sec.axis = sec_axis(~ (. +39)/100, name = 'GCC (-)')
  ) + 
  scale_x_datetime(
    name = NULL,
    limits = c(as.POSIXct("2023-08-01", tz = 'UTC'), as.POSIXct("2024-10-01", tz = 'UTC')),
    breaks = seq.POSIXt(from = as.POSIXct("2023-09-01", tz = 'UTC'), 
                        to = as.POSIXct("2024-10-01", tz = 'UTC'), 
                        by = "3 months") %>% with_tz('UTC'),
    date_labels = "%b '%y", 
    minor_breaks = "1 months") +
  coord_cartesian(expand = FALSE) +
  theme(legend.position = 'bottom',
        legend.title.position = 'top',
        legend.direction = 'horizontal',
        legend.background     = element_blank(),
        legend.key.size       = unit(0.4, "cm"),
        legend.key.spacing.y  = unit(0.01, "cm"),
        legend.spacing.x      = unit(-0.5, "cm"),
        legend.box.spacing = unit(0, "pt"),
        plot.margin = unit(c(0, 0.1, -0.5, 0.1), "cm"),
        legend.box.margin = unit(c(0, 0.1, -0.2, 0.1), "cm"),
  )
p_a

p_b <- ggplot(data = dat_plot) + 
  # -- annotate seasons and year change
  add_missing_fNH3() +
  add_season_rects(0.3, 0.33) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'), linewidth = 0.25) + 
  # -- Start leaf growth
  geom_vline(xintercept = as.POSIXct('24-05-2024', format='%d-%m-%Y', tz = "UTC"), linetype = 3) +
  geom_hline(yintercept = 0, linewidth = 0.5) + 
  # -- Plot LAI and NEE
  geom_line(aes(x = time.start,
                y = NEE.daily/1e3)) +
  geom_line(aes(x = time.start,
                y = NEE.daily.rm/1e3), color = RIVM_colors[1]) + 
  # Plot layout
  geom_text(
    data = data.frame(
      time.start = min(dat_plot$time.start, na.rm = TRUE),
      y = 0.28
    ),
    aes(x = time.start, y = y),
    label = "(b)",
    hjust = -0.1, vjust = 0.5,
    size = 3.5
  ) + 
  guides(color = guide_legend(ncol = 1)) + 
  scale_y_continuous(
    limits = c(-0.3,0.33),
    name = expression("NEE" ~ "(" * mmol ~ m^{-2} * day^{-1} *")")
  ) + 
  scale_x_datetime(
    name = NULL,
    limits = c(as.POSIXct("2023-08-01", tz = 'UTC'), as.POSIXct("2024-10-01", tz = 'UTC')),
    breaks = seq.POSIXt(from = as.POSIXct("2023-09-01", tz = 'UTC'), 
                        to = as.POSIXct("2024-10-01", tz = 'UTC'), 
                        by = "3 months") %>% with_tz('UTC'),
    date_labels = "%b '%y", 
    minor_breaks = "1 months") +
  coord_cartesian(expand = FALSE) +
  theme(legend.position = 'bottom',
        legend.title.position = 'top',
        legend.direction = 'vertical',
        legend.background     = element_blank(),
        legend.key.size       = unit(0.4, "cm"),
        legend.key.spacing.x  = unit(0.05, "cm"),
        legend.key.spacing.y  = unit(0.01, "cm"),
        legend.spacing.x      = unit(-0.5, "cm"),
        legend.box.spacing = unit(0, "pt"),
        legend.box.margin = unit(c(0, 0.1, -0.2, 0.1), "cm"),
        plot.margin = unit(c(0, 0.1, -0.5, 0.1), "cm"))
p_b

fig3 <- (p_a + p_b) 

# Save figure
ggsave(fig3,
       path = fig_path,
       file = 'Fig3.pdf', 
       width = wfull, 
       height = 0.6*wsingle, 
       units = "mm", dpi=600)
ggsave(fig3,
       path = fig_path,
       device=grDevices::png,
       file = 'Fig3.png', 
       width = wfull, 
       height = 0.6*wsingle, 
       units = "mm", dpi=600)
