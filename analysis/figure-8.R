##########' 
#' figure-8
##' 
#' Author: E.A. Melman @IRVM
#' Contributions by: 
##' 
#' Usage: Run. 
##' 
#' Aim: Compare GPP to effective exchange velocity (veff) for NH3 on daily and 
#' monthly scales
#########'
# Prep data ----
temp <- dat_Loobos_30 %>% 
  # Rename vars for convenience
  rename(Rg = BM.R.sw.in) %>% 
  # Select full year
  dplyr::filter(time.start >= as.POSIXct('2023-09-01 00:00:00', tz = 'UTC') & 
                  time.start < as.POSIXct('2024-09-01 00:00:00', tz = 'UTC')) %>% 
  # Get grouping variables
  mutate(Date = as.Date(time.start), 
         year_month = format(time.start, "%Y-%m"), 
         fNH3.flux = if_else(
           condition = fNH3.ff1, 
           true = fNH3.flux,
           false = NA),
         fNH3.ve = fNH3.flux/HT.cNH3)

# Function for summarizing at different time scales
ve_summary <- function(data, group_vars, date_var, res_label) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(
      fNH3.ve.daytime = if_else(Rg > 10, fNH3.ve, NA_real_),
      fNH3.ve.nighttime = if_else(Rg < 10, fNH3.ve, NA_real_)
    ) %>%
    summarize(
      GPP = sum(-GPP.gf * 1800 * 0.0001201 * 1e-3, 0, na.rm = TRUE),
      ve.all = mean(fNH3.ve, na.rm = TRUE),
      ve.daytime = mean(fNH3.ve.daytime, na.rm = TRUE),
      ve.nighttime = mean(fNH3.ve.nighttime, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    pivot_longer(
      cols = c(ve.all, ve.daytime, ve.nighttime),
      names_to = 'crit',
      values_to = 've'
    ) %>%
    mutate(
      res = res_label,
      month = if (date_var == "Date") month(!!sym(date_var)) else if (date_var == "year_month") month(ym(!!sym(date_var))) else {
        # For weekly, estimate month from week string
        y = as.integer(str_sub(!!sym(date_var), 1, 4))
        w = as.integer(str_sub(!!sym(date_var), 6, 7))
        as.integer(month(as.Date(paste0(y, "-01-01")) + weeks(w)))
      }
    )
}

# Calculate veff and GPP on different timescales
temp_daily   <- ve_summary(temp, c("season","Date"), "Date", "Daily")
temp_monthly <- ve_summary(temp, c("season","year_month"), "year_month", "Monthly")

# Combine dataframes
temp_NEE_ve <- temp_daily %>% 
  # bind_rows(temp_weekly) %>%
  bind_rows(temp_monthly) %>% 
  mutate(res = factor(res, levels = c('Daily', 'Monthly')))

# Plot results ----
fig8 <- ggplot(data = temp_NEE_ve, 
                   aes(x = GPP, 
                       y = ve)) + 
  # grid lines
  geom_hline(yintercept = 0) + 
  # Add points
  geom_point(aes(color = season)) +
  # linear fit
  # -- season season
  geom_smooth(data = temp_NEE_ve %>%
                dplyr::filter(
                  month %in% c(3,4,5,6,7,8,9)), ,
              method = 'lm',
              linetype = 2,
              color = 'black',
              linewidth = 0.5,
              se = FALSE) +
  stat_poly_eq(
    data = temp_NEE_ve %>%
      dplyr::filter(
        month %in% c(3,4,5,6,7,8,9)),
    aes(label = paste(..rr.label..), color = season),
    geom = "label",
    size = 2.5,
    formula = y ~ x,
    hjust = 0,
    label.x = "left",
    label.y = 0.02,
    parse = TRUE) +
  # -- Resting season
  geom_smooth(data = temp_NEE_ve %>%
                dplyr::filter(
                  !month %in% c(3,4,5,6,7,8,9)), ,
              method = 'lm',
              linetype = 2,
              color = 'black',
              linewidth = 0.5,
              se = FALSE) +
  stat_poly_eq(
    data = temp_NEE_ve %>%
      dplyr::filter(
        !month %in% c(3,4,5,6,7,8,9)),
    aes(label = paste(..rr.label..), color = season),
    geom = "label",
    size = 2.5,
    formula = y ~ x,
    hjust = 1,
    label.x = "right",
    label.y = -0.045,
    parse = TRUE) +
  # Plot layout
  scale_color_manual(values = RIVM_colors[c(1,2)]) + 
  labs(
    x = expression(Sigma*'GPP (ton C ha'^{-1}*')'),
    y = expression(bar(v[eff])~' (m s'^{-1}*')'),
    color = NULL,
  ) + 
  theme(legend.position = 'bottom', 
        legend.title.position = 'top',
        legend.spacing.x = unit(0.01, "cm"),
        legend.key.spacing.y  = unit(-0.1, "cm"),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.box.margin = unit(c(-0.6, 0.1, 0, 0.1), "cm"),
        plot.margin = unit(c(0.1, 0.1, -0.3, 0.1), "cm")) +
  lims(y = c(-0.05, 0.025)) + 
  facet_grid2(rows = vars(crit), cols = vars(res),
              scales = "free_x",
              labeller = as_labeller(
                c('ve.all' = 'All',
                  've.daytime' = 'Daytime',
                  've.nighttime' = 'Nighttime',
                  'Weekly' = 'Weekly',
                  'Daily' = 'Daily',
                  'Monthly' = 'Monthly')),
              strip = strip_themed(
                background_y = elem_list_rect(
                  fill = c(
                    "All" = '#D9D9D9',
                    "Daytime" = RIVM_colors[8], 
                    "Nighttime" = RIVM_colors[7]))))
fig8

# Save figure
ggsave(fig8,
       file = 'Fig8.pdf', 
       path = fig_path,
       width = wsingle, 
       height = 1.5*wsingle, 
       units = "mm", dpi=300)
ggsave(fig8,
       file = 'Fig8.png', 
       device=grDevices::png,
       path = fig_path,
       width = wsingle, 
       height = 1.5*wsingle, 
       units = "mm", dpi=300)
