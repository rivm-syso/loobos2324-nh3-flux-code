##########'
#' figure-5
##'
#' Author: E.A.Melman @RIVM
#' Contributions by:
##'
#' Usage: run. 
##'
#' Aim: Script to plot timeseries of concentration, flux and cumulative flux. 
#########'
# get temporary data ----
temp0 <- dat_Loobos_30 %>% 
  # Select relevant variables
  dplyr::select(time.start, yr, month, mD.cNH3, fNH3.ff1, fNH3.flux, 
                fNH3.flux.gf, 
                GPP.gf, LAI.gf, LAI, u.star, v.tke) %>% 
  # Filter data
  mutate(fNH3.flux = if_else(fNH3.ff1, fNH3.flux, NA),
         v.tke = if_else(u.star > 0.1, v.tke, NA))

# Prepare data for boxplots ---- 
temp <- temp0 %>% 
  # Format time
  mutate(month_year = format(time.start, "%Y-%m")) %>%
  # Group by month per year
  group_by(month_year) %>% 
  # Change timeformat to as.POSIXct and get sample size
  mutate(month_start = as.POSIXct(paste0(month_year, "-15")), 
         N_mD.cNH3 = sum(!is.na(mD.cNH3)),
         N_fNH3 = sum(!is.na(fNH3.flux)),
         N_v.tke = sum(!is.na(v.tke))) %>% 
  # Clear months with N < 50. 
  mutate(mD.cNH3 = ifelse(N_mD.cNH3 >= 50, mD.cNH3, NA),
         fNH3.flux = ifelse(N_fNH3 >= 50, fNH3.flux, NA))

# ---- Get stats for yticks
temp_ticks <- temp %>% 
  summarize(across(c(mD.cNH3, fNH3.flux, v.tke), 
                   list(p50 = ~ median(., na.rm=T)), 
                   .names = "{.col}.{.fn}")) %>% 
  ungroup() %>% 
  summarize(across(c(mD.cNH3.p50, fNH3.flux.p50, v.tke.p50), 
                   list(p50 = ~ median(., na.rm=T), 
                        min = ~ min(., na.rm=T),
                        max = ~ max(., na.rm=T)), 
                   .names = "{.col}.{.fn}"))  %>%
  pivot_longer(
    everything(),
    names_to = c("variable", "stat"),
    names_pattern = "(.+)\\.(p50|min|max)$"
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(cNH3 = mD.cNH3.p50, fNH3 = fNH3.flux.p50) %>%
  mutate(stat = factor(stat, levels = c("p50","min","max")))

# Plot results (timeseries) ----
# -- Helper functions for plot layout
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

# -- Plot Timeseries of [NH3] 
plot_A <- ggplot(data = temp, aes(x = time.start, y = mD.cNH3)) +
  geom_point(alpha=0) +
  # Annotate missing data and seasons --
  add_missing_cNH3() +
  add_season_rects(20, 21) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'),
             linewidth = 0.3) + 
  geom_boxplot(aes(x = month_start, y = mD.cNH3, group = month_year), color = 'grey25',
               fill = "grey90", outlier.shape = NA) + 
  common_x_scale +
  scale_y_continuous(limits = c(0, 21),
                     expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.1),
  ) +
  geom_text(
    data = data.frame(
      time.start = min(temp$time.start, na.rm = TRUE),
      y = 19.3
    ),
    aes(x = time.start, y = y),
    label = "(a)",
    hjust = 1.28, vjust = 0.5,
    size = 3.5
  ) + 
  labs(
    y = expression(paste("[NH"[3], "]"["mD"] ~ "("*mu*g~m^-3*")")),
    x = NULL) + 
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

# ---- Add marginal density
p1 <- ggMarginal(plot_A, type="histogram", margins = 'y', color = "grey25",
                 fill = "grey25", alpha = 0.3, size = 10, expand = FALSE)

# ---- Plot timeseries of fNH3 
plot_B <- ggplot(data = temp, aes(x=time.start, y = fNH3.flux)) + 
  geom_point(alpha=0) +
  # Annotate missing data and seasons --
  add_missing_cNH3() +
  annotate("rect", 
           xmin = as.POSIXct('2023-11-09 08:00:00', tz = 'UTC'),
           xmax = as.POSIXct('2024-02-09 13:30:00', tz = 'UTC'),
           ymin = -Inf, 
           ymax = Inf, 
           fill = 'grey80') + 
  add_season_rects(0.25, 0.2875) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'),
             linewidth = 0.3) + 
  geom_hline(yintercept = 0) + 
  geom_boxplot(aes(x = month_start, y = fNH3.flux, group = month_year), color = 'grey25',
               outlier.shape = NA, fill = "grey90") + 
  common_x_scale +
  scale_y_continuous(limits = c(-.5, 0.2875),
                     expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.01),
  ) +
  geom_text(
    data = data.frame(
      time.start = min(temp$time.start, na.rm = TRUE),
      y = 0.23
    ),
    aes(x = time.start, y = y),
    label = "(b)",
    hjust = 1.28, vjust = 0.5,
    size = 3.5
  ) + 
  labs(
    x = NULL,
    y = expression("F"["NH"[3]] ~ "("*mu * g ~ NH[3] ~ m^{-2} ~ s^{-1}*")")) + 
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

# ---- Add marginal density
p2 <- ggMarginal(plot_B, type="histogram", margins = 'y', color = "grey25",
                 fill = "grey25", alpha = 0.3, size = 10) 

# ---- Plot timeseries of fNH3 
plot_C <- ggplot(data = temp, aes(x=time.start, y = v.tke)) + 
  geom_point(alpha=0) +
  # Annotate missing data and seasons --
  add_missing_cNH3() +
  add_season_rects(3.5, 1.05*3.5) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'),
             linewidth = 0.3) + 
  geom_boxplot(aes(x = month_start, y = v.tke, group = month_year), color = 'grey25',
               outlier.shape = NA, fill = "grey90") + 
  common_x_scale + 
  scale_y_continuous(limits = c(0, 1.05*3.5),
                     expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.01),
                     # breaks = c(0,0.75, 1.01, 1.34, 3.5)
  ) +
  geom_text(
    data = data.frame(
      time.start = min(temp$time.start, na.rm = TRUE),
      y = 0.92*1.05*3.5
    ),
    aes(x = time.start, y = y),
    label = "(c)",
    hjust = 1.28, vjust = 0.5,
    size = 3.5
  ) + 
  labs(# tag = "(c)",
    x = NULL,
    y = expression("V"["TKE"] ~ "("*m ~ s^{-1}*")")) + 
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

# ---- Add marginal density
p3 <- ggMarginal(plot_C, type="histogram", margins = 'y', color = "grey25",
                 fill = "grey25", alpha = 0.3, size = 10) 

# -- Calculate cumulative sum
temp_cs <- temp0 %>% 
  # Select fluxes only 
  dplyr::select(time.start, yr, fNH3.flux, fNH3.flux.gf, GPP.gf, LAI.gf, LAI) %>% 
  # Calculate cumulative fluxes
  mutate(GPP.gf.cs = cumsum(-GPP.gf*conv_umolCO2m2_to_tonCha*conv_sec_to_halfhour),
         fNH3.flux.gf.cs = cumsum(fNH3.flux.gf*conv_ugNH3m2_to_kgNha*conv_sec_to_halfhour),
         fNH3.flux.gf.lb = cumsum((1 + 0.14)*fNH3.flux.gf*conv_ugNH3m2_to_kgNha*conv_sec_to_halfhour),
         fNH3.flux.gf.ub = cumsum((1 - 0.14)*fNH3.flux.gf*conv_ugNH3m2_to_kgNha*conv_sec_to_halfhour),
         # Calculate cumflux of observed fluxes, deal with NA
         fNH3.flux = if_else(condition = is.na(fNH3.flux), 
                             true = 0, 
                             false = fNH3.flux),
         fNH3.flux.cs = cumsum(fNH3.flux * conv_ugNH3m2_to_kgNha * conv_sec_to_halfhour),
         fNH3.flux.cs = if_else(condition = fNH3.flux == 0, 
                                true = NA_real_, 
                                false = fNH3.flux.cs))

# ---- Plot cumulative flux
plot_D <- ggplot(data = temp_cs) + 
  # Annotate missing data and seasons --
  add_missing_fNH3() +
  add_season_rects(3, 1.05*3) +
  geom_vline(xintercept = as.POSIXct('2024-01-01 00:00:00', tz = 'UTC'),
             linewidth = 0.3) + 
  # Plot data
  geom_point(aes(x = time.start, y = (3/40) * (fNH3.flux.cs + 40), 
                 color = 'fNH3.flux.cs'), size = 0.5) + 
  geom_ribbon(aes(x = time.start, 
                  ymin = (3/40) * (fNH3.flux.gf.lb + 40), 
                  ymax = (3/40) * (fNH3.flux.gf.ub + 40), 
                  fill = 'fNH3.gf'),
              alpha = 0.3, ) + 
  geom_line(aes(x = time.start, 
                y = (3/40) * (fNH3.flux.gf.cs + 40), 
                color = 'fNH3.gf')) + 
  geom_line(aes(x = time.start, 
                y = (3/40) * (GPP.gf.cs + 40), 
                color = 'GPP')) + 
  geom_line(aes(x = time.start, 
                y = LAI.gf, 
                color = 'LAI')) + 
  geom_point(data = dat_Loobos_30 %>%
               dplyr::filter(time.start != as.POSIXct('24-05-2024', format='%d-%m-%Y', tz = "UTC")),
             aes(x = time.start,
                 y = LAI,
                 color = 'LAI'), shape = 17) + 
  # Plot layout
  geom_text(
    data = data.frame(
      time.start = min(temp_cs$time.start, na.rm = TRUE),
      y = 0.92*1.05*3
    ),
    aes(x = time.start, y = y),
    label = "(d)",
    hjust = 1.28, vjust = 0.5,
    size = 3.5
  ) + 
  common_x_scale + 
  scale_y_continuous(
    name = expression(LAI ~ '(m'^{2}*m^{-2}*')'),
    limits = c(0, 1.05*3),
    expand = c(0,0),
    breaks = c(2.2, 2.8),
    sec.axis = sec_axis(
      ~ (-40 + (40/3)*.),
      name = expression(
        atop(
          Sigma*F[~NH[3]]~"("*kg~N~ha^{-1}*")",
          Sigma*GPP~"(ton C"~ha^{-1}*")"
        )
      ),
      breaks = c(-33.0, -19.6, -13.6, 0)
    )
  ) +
  scale_color_manual(name = 'flux', 
                     values = c('fNH3.flux.cs' = "grey25", 
                                'fNH3.gf' = 'grey25',
                                'GPP' = RIVM_colors[4], 
                                'LAI' = RIVM_colors[5]),
                     labels = c('fNH3.flux.cs' = expression(F[~NH[3]]), 
                                'fNH3.gf' = expression(F[~NH[3]*',gf']),
                                'GPP' = expression('GPP'['gf']))
  ) +
  scale_fill_manual(name = 'flux', 
                    values = c('fNH3.gf' = 'grey25'),
                    labels = c('fNH3.gf' = expression(F[~NH[3]*',gf']))
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        size = 2,
        fill = c(NA, "grey25", NA, NA)
      )
    ),
  ) + 
  labs(x = NULL) + 
  theme(legend.position = c(0.13, 0.4),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(),
        legend.background = element_blank()) 
plot_D

p4 <- ggMarginal(plot_D, type="histogram", margins = 'y', color = 'white',
                 fill = 'white', alpha = 0.0, size = 150)

# ---- Build figure
fig5 <- plot_grid(p1, p2, p3, p4,
                    ncol = 1,
                    align="vh", axis = "tb")

# ---- Save figure
ggsave(fig5,
       file = 'Fig5.png', 
       path = fig_path,
       width = wfull, 
       height = 1.4*wdouble, 
       units = "mm", dpi=300,
       device=grDevices::png)
ggsave(fig5,
       file = 'Fig5.pdf', 
       path = fig_path,
       width = wfull, 
       height = 1.4*wdouble, 
       units = "mm", dpi=300)


