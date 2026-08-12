##########' 
#' figure-12
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage: Run. 
##' 
#' Aim: lot diurnal cycle of effective exchange velocity for NH3 (veff) grouped
#' by comparable months. NOTE: this code only shows the data for Loobos.
#########' 
# Monthly averages ----
# -- Define clips
y_min <- -0.05
y_max <- 0.03

# -- Calculate dc's
temp_dc <- dat_Loobos_30 %>% dplyr::filter(!month %in% c(1,12)) %>%
  # Adjust timestamp 
  mutate(hr = hour(time.start),
         mn = minute(time.start)) %>% 
  # Calculate veff and filter data first
  mutate(
    veff = if_else(condition = fNH3.ff2 == 1, 
                   true = fNH3.flux/HT.cNH3,
                   false = NA),
    group = case_when(
      month %in% c(2, 10, 11) ~ "Oct/Nov/Feb",
      month %in% 3:4 ~ "Mar/Apr",
      month %in% 5:6 ~ "May/Jun",
      month %in% 7:8 ~ "Jul/Aug",
      month %in% 9 ~ "Sep",
      TRUE ~ NA_character_  # For any other values, return NA 
    ), 
    month = factor(group, 
                   levels = c("Oct/Nov/Feb", "Mar/Apr", "May/Jun", 
                              "Jul/Aug", "Sep"))) %>% 
  group_by(month, hr, mn) %>% 
  summarize(across(veff,
                   list(pp25 = ~ quantile(., probs = 0.25, na.rm = TRUE),
                        pp50 = ~ quantile(., probs = 0.50, na.rm = TRUE),
                        pp75 = ~ quantile(., probs = 0.75, na.rm = TRUE), 
                        ppN = ~ sum(!is.na(.))),
                   .names = "{col}.{fn}")) %>% 
  # Define hhTime for xticks
  mutate(
    hhTime = ymd_hms(paste0(today("UTC"), " ", hr, ":", mn,":00"))) %>%
  pivot_longer(
    cols = starts_with('veff'),
    names_to = c("fluxcol", "quantile"),
    names_sep = "\\.p",
    values_to = "value") %>%
  pivot_wider(
    names_from = quantile,
    values_from = value) %>% 
  mutate(p25_clipped = pmin(pmax(p25, y_min), y_max),
         p50_clipped = pmin(pmax(p50, y_min), y_max),
         p75_clipped = pmin(pmax(p75, y_min), y_max))

# Plot results ----
# -- Define xticks range
tstart <- as.POSIXct(paste0(Sys.Date(), "00:00:00"), tz = "UTC")
tend <- as.POSIXct(paste0(Sys.Date(), "24:00:00"), tz = "UTC")

# -- Make plot
Fig12 <- ggplot(data = temp_dc) + 
  geom_hline(yintercept = 0) +
  # ---- p25 - p75 uncertainty range
  geom_ribbon(aes(hhTime,             
                  ymin = p25_clipped, 
                  ymax = p75_clipped, 
                  fill = "Loobos"), alpha = 0.3) + 
  # # ---- Mid value (p50)
  geom_line(aes(x = hhTime,
                y = p50_clipped,
                linewidth = pN,
                colour = "Loobos"), 
            lineend = 'round') +
  # ---- Plot properties
  # lims(y = c(-0.4, 0.1)) + 
  scale_linewidth_binned(name = '#', 
                         range = c(0.5, 2), 
                         breaks = c(0, 5, 10, 20)) + 
  scale_color_manual(name = 'Site', 
                     values = c('grey25')) +
  scale_fill_manual(name = 'Site', 
                    values = c('grey25')) +
  scale_x_datetime(breaks = seq(from = tstart,
                                to = tend,
                                by = "6 hours"),
                   date_labels = "%H",
                   minor_breaks = "6 hours") +
  coord_cartesian(expand = FALSE) +
  labs(
    x = 'Hour of day', 
    y = expression(v[eff]~' (m s'^{-1}*')')) + 
  facet_wrap(~ month, ncol = 5) +
  theme(legend.position = 'bottom',
        legend.title.position = 'top',
        legend.box.spacing = unit(0, 'mm')
  )
Fig12

# -- Save figure
ggsave(Fig12,
       file = 'Fig12.pdf', 
       path = fig_path,
       width = wfull, 
       height = 0.5*wsingle, 
       units = "mm", dpi=600)
ggsave(Fig12,
       path = fig_path,
       device=grDevices::png,
       file = 'Fig12.png', 
       width = wsingle, 
       height = 0.8*wsingle, 
       units = "mm", dpi=600)
