##########'
#' figure-S3
##'
#' Author: E.A.Melman @RIVM
#' Contributions by:
##'
#' Usage:
##'
#' Aim: Script to plot diurnal cycle of flux and concentration
#########'
# Make temporary data structure ----
temp <- dat_Loobos_30 %>% 
  # Select relevant variables
  select(time.start, season, month, mD.cNH3)  %>% 
  # Filter data
  mutate(
    hr = hour(time.start),
    mn = minute(time.start),
    month = factor(format(time.start, "%B"),
                   levels = c('January','February', 'March', 'April', 'May', 'June', 
                              'July', 'August', 'September', 'October', 
                              'November', 'December'))) %>% 
  # Group data
  group_by(month, hr, mn) %>% 
  # Summarize results
  summarize(across(mD.cNH3,
                   list(pp25 = ~ quantile(., probs = 0.25, na.rm = TRUE),
                        pp50 = ~ quantile(., probs = 0.50, na.rm = TRUE),
                        pp75 = ~ quantile(., probs = 0.75, na.rm = TRUE), 
                        ppN = ~ sum(!is.na(.))),
                   .names = "{col}.{fn}")) %>% 
  # Define hhTime for xticks
  mutate(hhTime = ymd_hms(paste0(today("UTC"), " ", hr, ":", mn,":00"))) %>%
  pivot_longer(
    cols = starts_with("mD.cNH3"),
    names_to = c("fluxcol", "quantile"),
    names_sep = "\\.p",
    values_to = "value") %>%
  pivot_wider(
    names_from = quantile,
    values_from = value) %>% 
  drop_na()

# Plot data ----
# ---- Define xticks
tstart <- as.POSIXct(paste0(Sys.Date(), "00:00:00"), tz = "UTC")
tend <- as.POSIXct(paste0(Sys.Date(), "24:00:00"), tz = "UTC")

# ---- Concentration plot
FigS3 <- ggplot(data = temp) + 
  geom_line(aes(x = hhTime, y = p50), color = 'grey25',lineend = "round") + 
  geom_ribbon(aes(x = hhTime,
                  ymin = p25,
                  ymax = p75), fill = 'grey25', alpha=0.3) +
  # Plot formatting
  scale_x_datetime(breaks = seq(from = tstart, 
                                to = tend, 
                                by = "6 hours"),
                   date_labels = "%H", 
                   minor_breaks = "2 hours") +
  coord_cartesian(expand = FALSE) +
  lims(y = c(0,18)) + 
  labs(x = 'Hour of day',
       y = expression(paste("[NH"[3], "]"["mD"] ~ "("*mu*g~m^-3*")"))) + 
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
  theme()
FigS3

# ---- Save figures
ggsave(FigS3,
       file = 'FigS3.png', 
       path = fig_path,
       width = wfull, 
       height =  1.05*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)
ggsave(FigS3,
       file = 'FigS3.pdf', 
       path = fig_path,
       width = wfull, 
       height =  1.05*wsingle, 
       units = "mm", dpi=600)
