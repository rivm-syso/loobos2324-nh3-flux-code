##########' 
#' table-S2
##' 
#' Author: E.A. Melman @RIVM
#' Contributions by: 
##' 
#' Usage:  
##' 
#' Aim: calculate exchange efficiency (see aslo figure-7)
#########' 
# Test statistical siginficance and print table ----
dat_Loobos_30 %>%
  select(time.start, yr, month, season,
         Observed = fNH3.flux, fNH3.ff2) %>%
  dplyr::filter(fNH3.ff2) %>%
  # Make and factor timestamps
  mutate(
    hr = hour(time.start),
    mn = minute(time.start),
    time = factor(hr + mn/60),
    month = factor(month)) %>%
  # run ANOVA
  {aov(Observed ~ month * time, data = .)} %>% 
  anova() %>%     
  # extract ANOVA table
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Term") %>%
  # add significance stars
  mutate(across(where(is.numeric), round, 4),
         Signif = case_when(                                   
           !is.na(`Pr(>F)`) & `Pr(>F)` < 0.001 ~ "***",
           !is.na(`Pr(>F)`) & `Pr(>F)` < 0.01  ~ "**",
           !is.na(`Pr(>F)`) & `Pr(>F)` < 0.05  ~ "*",
           TRUE ~ ""
         )) %>%
  # Print table
  knitr::kable(format = "simple",
        booktabs = TRUE, 
        caption = "Two-way ANOVA of Observed NH3 flux across months and hours, 
        including the month × hour interaction to test whether the diurnal cycle 
        shape differs between months.",
        linesep = "", row.names = FALSE) 
