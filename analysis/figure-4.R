##########'
#' figure-4
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage:
##'
#' Aim: Show turbulent properties throughout measurement period, as function 
#' against wind direcution and their diurnal evolution
#########'
# Get actual turbulence ----
temp <- dat_Loo_export %>% 
  select(time.start, month, BM.WD.382, fNH3.flux, 
         HT.cNH3, fNH3.ff2, v.tke, ell) %>% 
  mutate(
    # Exchange velocity
    fNH3.ve = fNH3.flux/HT.cNH3,
    # Filter deposition velocity
    fNH3.ve = ifelse(fNH3.ff2, fNH3.ve, NA),
    # Grouping vars
    hr = hour(time.start),
    mn = minute(time.start)
    ) %>%
  # Make bins
  mutate(WD.bins = cut(BM.WD.382, breaks = seq(0,360,20)),
         WD.binmid = as.numeric(WD.bins) * 20 - 10)

# Plot wind direction ----
# -- Prep data 
temp_WD <- temp %>%
  group_by(WD.binmid) %>% 
  summarize(across(c(ell, v.tke, fNH3.ve, HT.cNH3),
                   list(pp25 = ~ quantile(., probs = 0.25, na.rm = TRUE),
                        pp50 = ~ quantile(., probs = 0.50, na.rm = TRUE),
                        pp75 = ~ quantile(., probs = 0.75, na.rm = TRUE), 
                        ppN = ~ sum(!is.na(.))),
                   .names = "{col}.{fn}"))

# Make long
temp_WD_long <- temp_WD %>%
  select(WD.binmid,
         ell.pp25, ell.pp50, ell.pp75,
         v.tke.pp25, v.tke.pp50, v.tke.pp75,
         HT.cNH3.pp25, HT.cNH3.pp50, HT.cNH3.pp75,
         fNH3.ve.pp25, fNH3.ve.pp50, fNH3.ve.pp75) %>%
  pivot_longer(
    cols = -WD.binmid,
    names_to = c("var", ".value"),
    names_pattern = "(.*)\\.(pp[0-9]{2})"
  ) %>%
  mutate(var = factor(var, levels = c("v.tke", "ell", "fNH3.ve", "HT.cNH3")))

# -- Faceted polar plot
p_a <- ggplot(temp_WD_long) + 
  geom_ribbon(aes(x = WD.binmid, ymin = pp25, ymax = pp75), alpha = 0.3) +
  geom_line(aes(x = WD.binmid, y = pp50), lineend = 'round') +
  coord_polar(start = 0) +
  scale_x_continuous(
    name = NULL,
    limits = c(0,360),
    breaks = seq(0, 360, 90)
  ) +
  labs(y = NULL) +
  facet_wrap(
    ~ var,
    scales = "free_y",
    ncol = 4,
    labeller = labeller(
      var = as_labeller(
        c(
          "v.tke"   = "(a)~V[TKE]~'('*m~s^-1*')'",
          "ell"       = "(b)~'\u2113'[m]~'(m)'",
          "HT.cNH3" = "(d)~'['*NH[3]*']'['HT']~'('*mu*g~m^-3*')'",
          "fNH3.ve" = "(c)~v[eff]~'('*m~s^-1*')'"
        ),
        label_parsed
      )
    )
  ) +
  theme(
    panel.grid.major = element_line(colour = "grey80"),
    panel.grid.minor = element_line(colour = "grey90"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(1, 'mm')
  )

# Plot diurnal cycle ----
# -- Define xticks
tstart <- as.POSIXct(paste0(Sys.Date(), "00:00:00"), tz = "UTC")
tend   <- as.POSIXct(paste0(Sys.Date(), "24:00:00"), tz = "UTC")

# Prep data
temp_dc <- temp %>% 
  mutate(hhTime = ymd_hms(paste0(today("UTC"), " ", hr, ":", mn,":00"))) %>% 
  group_by(hhTime) %>% 
  summarize(across(
    c(ell, v.tke, fNH3.ve, HT.cNH3),
    list(
      pp25 = ~ quantile(., probs = 0.25, na.rm = TRUE),
      pp50 = ~ quantile(., probs = 0.50, na.rm = TRUE),
      pp75 = ~ quantile(., probs = 0.75, na.rm = TRUE), 
      ppN  = ~ sum(!is.na(.))
    ),
    .names = "{col}.{fn}"
  )) %>% 
  select(hhTime, 
         ell.pp25, ell.pp50, ell.pp75,
         v.tke.pp25, v.tke.pp50, v.tke.pp75,
         HT.cNH3.pp25, HT.cNH3.pp50, HT.cNH3.pp75,
         fNH3.ve.pp25, fNH3.ve.pp50, fNH3.ve.pp75) %>%
  pivot_longer(
    cols = -hhTime,
    names_to = c("var", ".value"),
    names_pattern = "(.*)\\.(pp[0-9]{2})"
  ) %>%
  mutate(var = factor(var, levels = c("v.tke", "ell", "fNH3.ve", "HT.cNH3")))

# Make faceted plot
p_a <- ggplot(temp_dc) + 
  geom_ribbon(aes(x = hhTime, ymin = pp25, ymax = pp75), alpha = 0.3) +
  geom_line(aes(x = hhTime, y = pp50), lineend = "round") +
  coord_cartesian(expand = FALSE) +
  scale_x_datetime(
    name = "Hour of day",
    breaks = seq(from = tstart, to = tend, by = "6 hours"),
    date_labels = "%H",
    minor_breaks = "6 hours"
  ) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~var, 
             scales = "free_y", 
             ncol = 4,
             labeller = as_labeller(
               c(
                 "HT.cNH3" = "(h)~'['*NH[3]*']'['HT']~'('*mu*g~m^-3*')'",
                 "v.tke"   = "(e)~V[TKE]~'('*m~s^-1*')'",
                 "ell"       = "(f)~'\u2113'[m]~'(m)'",
                 "fNH3.ve" = "(g)~v[eff]~'('*m~s^-1*')'"
               ),
               label_parsed
             )) +
  theme()
p_a

# Combine figure
fig4 <- plot_grid(
  p_a, p_a,
  ncol = 1, 
  align = 'v',
  axis = 'tb'
)

# Build and save plot ----
ggsave(
  fig4,
  # Saving settings
  file = 'Fig4.pdf', 
  device = cairo_pdf,
  path = fig_path,
  width = wfull, 
  height = 1*wsingle, 
  units = "mm", dpi=300)
ggsave(
  fig4,
  # Saving settings
  file = 'Fig4.png',
  device=grDevices::png,
  path = fig_path,
  width = wfull, 
  height = 1*wsingle, 
  units = "mm", dpi=300)
