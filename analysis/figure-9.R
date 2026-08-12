##########'
#' figure-9
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage:
##'
#' Aim: Plot timeseries of veff, GPP, LAI and GCC
#########' 
# Get data ----
# -- veff data
temp_monthly <- dat_Loobos_30 %>%
  # Rename vars
  rename(Rg = BM.R.sw.in) %>% 
  # Select full year
  dplyr::filter(time.start >= as.POSIXct('2023-09-01 00:00:00', tz = 'UTC') & 
                  time.start < as.POSIXct('2024-09-01 00:00:00', tz = 'UTC')) %>% 
  # Get grouping variables
  mutate(year_month = format(time.start, "%Y-%m"), 
         fNH3.flux = if_else(
           condition = fNH3.ff1, 
           true = fNH3.flux,
           false = NA),
         fNH3.ve = fNH3.flux/HT.cNH3) %>% 
  # Calculate monthly values
  group_by(year_month) %>% 
  # Calculate exchange velocity
  mutate(
    fNH3.ve.daytime = if_else(
      condition = Rg > 10, 
      true = fNH3.ve,
      false = NA
    ),
    fNH3.ve.nighttime = if_else(
      condition = Rg < 10, 
      true = fNH3.ve,
      false = NA
    )) %>% 
  # Get monthly values
  summarize(
    GPP = sum(-GPP.gf*conv_sec_to_halfhour*conv_umolCO2m2_to_tonCha,0, na.rm=TRUE),
    ve.daytime = mean(fNH3.ve.daytime, na.rm=T),
    ve.nighttime = mean(fNH3.ve.nighttime, na.rm=T)) %>% 
  ungroup() %>% 
  # Collect fNH3 for different conditions
  pivot_longer(
    cols = c(ve.daytime, ve.nighttime),
    names_to = 'crit',
    values_to = 've'
  ) %>% 
  # Get month number
  mutate(month = month(ym(year_month)))

# Plot yearly cycle
Fig9 <- ggplot(data = dat_Loobos_30) + 
  # -- annotate seasons and year change
  add_missing_fNH3() +
  add_season_rects(3.3, 3.5) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'), linewidth = 0.25) + 
  # -- Start leaf growth
  geom_vline(xintercept = as.POSIXct('24-05-2024', format='%d-%m-%Y', tz = "UTC"), linetype = 3) +
  # -- Plot LAI
  geom_line(aes(x = time.start,
                y = LAI.gf,
                color = 'LAI')) +
  geom_point(data = dat_Loobos_30 %>%
               dplyr::filter(time.start != as.POSIXct('24-05-2024', format='%d-%m-%Y', tz = "UTC")),
             aes(x = time.start,
                 y = LAI, 
                 color = 'LAI')) +
  # -- Plot GCC
  geom_line(aes(x = time.start,
                y = GCC*100-39,
                color = 'GCC')) +
  # -- plot monthly veff
  geom_point(data = temp_monthly %>%
               mutate(time.mid = as.POSIXct(paste0(year_month, '-15'), format = '%Y-%m-%d')),
             aes(x = time.mid,
                 y = ve*100 + 3,
                 color = crit
             ),
             shape = 4) +
  geom_line(data = temp_monthly %>%
              mutate(time.mid = as.POSIXct(paste0(year_month, '-15'), format = '%Y-%m-%d')) %>%
              dplyr::filter(month %in% c(3:8)),
            aes(x = time.mid,
                y = ve*100 + 3,
                color = crit),
            linetype = 2) +
  # -- plot monthly GPP
  geom_line(data = temp_monthly %>% 
              mutate(time.mid = as.POSIXct(paste0(year_month, '-15'), format = '%Y-%m-%d')), 
            aes(x = time.mid,
                y = -GPP, 
                color = 'GPP')) + 
  geom_point(data = temp_monthly %>% 
               mutate(time.mid = as.POSIXct(paste0(year_month, '-15'), format = '%Y-%m-%d')), 
             aes(x = time.mid,
                 y = -GPP, 
                 color = 'GPP')) + 
  # Plot layout
  scale_color_manual(
    name = NULL,
    values = c(
      'LAI' = 'black',
      've.daytime' = RIVM_colors[8],
      've.nighttime' = RIVM_colors[7],
      'GPP' = RIVM_colors[4],
      'GCC' = RIVM_colors[9]),
    breaks = c(
      've.daytime', 've.nighttime', 'GCC', 'GPP', 'LAI'),
    labels = c('ve.all' = expression('All'~bar(v[eff])),
               've.daytime' = expression('Daytime'~bar(v[eff])),
               've.nighttime' = expression('Nighttime'~bar(v[eff])))) +
  guides(color = guide_legend(ncol = 3)) + 
  scale_y_continuous(
    limits = c(0,3.5),
    name = expression(atop(-Sigma*'GPP (ton C ha'^{-1}*month^{-1}*')',"LAI (" * m^{2} * m^{-2} *")")),
    sec.axis = sec_axis(~ (. -3)/100, name = expression(bar(v[eff])~' (m s'^{-1}*')'))) + 
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
        legend.spacing.x = unit(0.01, "cm"),
        legend.key.spacing.y  = unit(-0.1, "cm"),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.box.margin = unit(c(-0.6, 0.1, 0, 0.1), "cm"),
        plot.margin = unit(c(0.1, 0.1, -0.3, 0.1), "cm"))
Fig9

# Save figure
ggsave(Fig9,
       path = fig_path,
       file = 'Fig9.pdf', 
       width = wsingle, 
       height = 0.8*wsingle, 
       units = "mm", dpi=600)
ggsave(Fig9,
       path = fig_path,
       device=grDevices::png,
       file = 'Fig9.png', 
       width = wsingle, 
       height = 0.8*wsingle, 
       units = "mm", dpi=600)
