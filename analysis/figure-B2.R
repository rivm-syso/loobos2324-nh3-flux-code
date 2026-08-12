##########'
#' figure-B2
##'
#' Author: E.A.Melman @RIVM and M.Voorneveld @RIVM
#' Contributions by: 
##'
#' Usage: Run xgb-gapfilling.R first! 
##'
#' Aim: Plot feature importance of XGB model
#########' 
# Compute importance matrices ----
# -- for low u.star threshold
importance_matrix_l <- xgb.importance(
  feature_names = colnames(df_reg_xg_l %>% select(-c(fNH3.flux, time.start))),
  model = model_xg_fNH3_l$finalModel
)

# -- for high u.star threshold
importance_matrix_h <- xgb.importance(
  feature_names = colnames(df_reg_xg_h %>% select(-c(fNH3.flux, time.start))),
  model = model_xg_fNH3_h$finalModel
)

# -- Combine importance matrices
im <- bind_rows(
  importance_matrix_h %>% drop_na() %>% mutate(Threshold = "high"),
  importance_matrix_l %>% drop_na() %>% mutate(Threshold = "low")) %>%
  # Set and sort Feature levels based on top 'high' threshold feature
  mutate(Feature = factor(Feature, levels = unique(Feature[Threshold == 'high'][order(-Gain[Threshold == 'high'])])))

# Plot feature importance ----
FigB2 <- ggplot(data = im, aes(x = Feature, y = Gain, fill = Threshold)) + 
  geom_col(position = "dodge") +
  scale_x_discrete(labels = c(
    "mD.cNH3"     = expression("[NH"[3]*"]"['mD']),
    "BM.RH.382"   = expression('RH'['38.2m']),
    "LAI"         = "LAI",
    "WDx.bins"    = expression('WD'[x]),
    "cNH3.aveprev"= expression("[NH"[3]*"]"['mD, 6d']),
    'NEE.gf'      = 'NEE',
    "BM.Tsurf"    = expression('T'['surf']),
    "WDy.bins"    = expression('WD'[y]),
    "u.star"      = expression('u'['*']),
    "BM.LW.157"   = expression('LW'['15.7m']),
    "SL.SWC.005"  = expression('SWC'['5cm']),
    "LE"          = "LE",
    "zoL"         = expression(zeta),
    "BM.WS.382"   = "U",
    "T.star"      = expression(theta['*']),
    "BM.Tair.382" = expression('T'['air']),
    "GPP.gf"      = "GPP",
    "BM.VPD.382"  = expression('VPD'['38.2m']),
    "H"           = "H",
    "h.abl"       = expression('h'['abl']),
    "BM.R.sw.in"  = expression('R'[g]),
    "v.tke"       = expression('V'['TKE']),
    # fallback to plain text for others
    .default = waiver()
  )) +
  ggbreak::scale_y_break(c(0.15, 0.4), 
                         expand = FALSE) +
  lims(y = c(0,0.6)) +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom') +
  scale_fill_manual(
    values = c('low' = RIVM_colors[4],
               'high' = RIVM_colors[8]), 
    labels = c('low' = 'XGB-low',
               'high' = 'XGB-high')
  )

# Save figure 
ggsave(FigB2,
       file = 'FigB2.pdf', 
       path = fig_path,
       width = 1*wsingle, 
       height = 0.9*wsingle, 
       units = "mm", dpi=600)
ggsave(FigB2,
       file = 'FigB2.png', 
       path = fig_path,
       width = 1*wsingle, 
       height = 0.9*wsingle, 
       units = "mm", dpi=600,
       device=grDevices::png)
