##########'
#' figure-11
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage:
##'
#' Aim: Plot probability density functions of (a) HT.cNH3, (b) veff and (c) fNH3. 
#' *NOTE*: this code only shows the data for Loobos. 
#########'
# Collect data ----
temp <- dat_Loobos_30 %>% 
  dplyr::rename(observed = fNH3.flux) %>% 
  # Filter data
  mutate(across(-c(time.start, fNH3.ff2, HT.cNH3), ~ if_else(
    condition = fNH3.ff2 == TRUE, 
    true = ., 
    false = NA)),
    fNH3.ve = observed/HT.cNH3)

# Make figures ----
p_a <- ggplot(data = temp, aes(x = HT.cNH3, color = 'Loobos', fill = 'Loobos')) + 
  geom_density(alpha = 0.3, binwidth = 0.5, position = 'identity', bounds = c(0, Inf)) + 
  lims(x = c(0,20)) +
  coord_cartesian(expand = FALSE) + 
  scale_color_manual(
    values = c('Loobos' = 'grey25'),
    labels = c('Loobos' = 'Loobos')
  ) + 
  scale_fill_manual(
    values = c('Loobos' = 'grey25'),
    labels = c('Loobos' = 'Loobos')
  ) + 
  lims(y = c(0,0.1501)) +
  labs(tag = '(a)', 
       y = 'Density',
       color = NULL, fill = NULL,
       x = expression(paste("[NH"[3]*"]" ~ "("*mu*g~m^-3*")")))

p_b <- ggplot(data = temp, aes(x = fNH3.ve, color = 'Loobos', fill = 'Loobos')) + 
  geom_vline(xintercept = 0) + 
  geom_density(alpha = 0.3, binwidth = 0.02, position = 'identity') + 
  lims(x = c(-.1, 0.1),
       y = c(0,35)) +
  coord_cartesian(expand = FALSE) +
  scale_color_manual(
    values = c('Loobos' = 'grey25'),
    labels = c('Loobos' = 'Loobos')
  ) + 
  scale_fill_manual(
    values = c('Loobos' = 'grey25'),
    labels = c('Loobos' = 'Loobos')
  ) + 
  labs(tag = '(b)', 
       color = NULL, fill = NULL, y = NULL ,
       x = expression(paste("v"["e"] ~ "("*m~s^-1*")"))) + 
  theme(legend.position = 'right')

p_c <-  ggplot(data = temp, aes(x = observed, color = 'Loobos', fill = 'Loobos')) + 
  geom_vline(xintercept = 0) + 
  geom_density(alpha = 0.3, binwidth = 0.02, position = 'identity') + 
  lims(x = c(-.5, 0.25),
       y = c(0,7.01)) +
  coord_cartesian(expand = FALSE) +
  scale_color_manual(
    values = c('Loobos' = 'grey25'),
    labels = c('Loobos' = 'Loobos')
  ) + 
  scale_fill_manual(
    values = c('Loobos' = 'grey25'),
    labels = c('Loobos' = 'Loobos')
  ) + 
  labs(tag = '(c)', 
       color = NULL, fill = NULL,  
       x = expression(paste("F"["NH"[3]] ~ "("*mu*g~m^-2~s^-1*")"))) + 
  theme(legend.position = 'right',
        legend.direction = 'vertical')

# -- Build plot
Fig11 <- 
  (p_a + labs(y = "Density")) + 
  (p_b + labs()) + 
  (p_c + labs(y = NULL)) + 
  plot_layout(guides = "collect") & theme(legend.position = "right")

# -- Save figure
ggsave(Fig11,
       file = 'Fig11.pdf', 
       path = fig_path,
       width = wfull, 
       height = 0.7*wsingle, 
       units = "mm", dpi=600)
ggsave(Fig11,
       path = fig_path,
       device=grDevices::png,
       file = 'Fig11.png', 
       width = wsingle, 
       height = 0.8*wsingle, 
       units = "mm", dpi=600)
  

