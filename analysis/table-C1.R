########## 
# table-C1
## 
# Author: E.A. Melman @RIVM
# Contributions by: 
## 
# Usage: 
## 
# Aim: Statistical comparison of observed and DEPAC predicted NH3-flux
######### 
# Collect data ----
temp0 <- dat_Loobos_30 %>% 
  # Select relevant variables
  dplyr::select(
    time.start, fNH3.ff2,
    Observed = fNH3.flux, DEPAC = fNH3.DEPAC, 
  ) %>%
  rowwise() %>%
  # Filter data
  mutate(
    across(c(Observed, DEPAC), 
           ~ if_else(
             condition = fNH3.ff2 == TRUE & all(!is.na(c(Observed, DEPAC))), 
             true = ., 
             false = NA_real_)))

# Evaluate DEPAC performance ----
temp0 %>% 
  dplyr::select(Observed, DEPAC) %>% 
  drop_na() %>%
  mutate(
    res = Observed - DEPAC
  ) %>% 
  summarise(
    DEPAC_n = sum(!is.na(DEPAC)),
    DEPAC_rmse = sqrt(mean(res^2)),
    DEPAC_mae = mean(abs(res)),
    DEPAC_MB = 1/DEPAC_n*sum((-res)),
    DEPAC_NSE = 1 - sum(res^2) / sum((Observed - mean(Observed))^2),
    DEPAC_avg = mean(DEPAC),
    DEPAC_p50 = median(DEPAC),
    Observed_n = sum(!is.na(Observed)),
    Observed_avg = mean(Observed),
    Observed_p50 = median(Observed)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>% 
  knitr::kable(
    digits = 2, format = "simple", 
    caption = "Comparison of Observed and DEPAC predicted NH3-flux.",
    booktabs = TRUE
  )
