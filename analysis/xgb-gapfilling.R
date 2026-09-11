##########'
#' xgb-gap-filling
##'
#' Author: E.A.Melman @RIVM and M.Voorneveld @RIVM
#' Contributions by: 
##'
#' Usage: *NOTE* The results produced with XGB model are only suitable for estimating 
#' the annual deposition load, and should not be used for process studies. 
##'
#' Aim: Gap-fill NH3 flux data using XGBoosting
#########'
# Collect input data ----
df_xg_fNH3 <- dat_Loobos_30 %>% 
  mutate(
    # Calculate average SWC at 5cm depth
    SL.SWC.005 = rowMeans(select(., SL.SWC.W.005, SL.SWC.N.005, SL.SWC.E.005, SL.SWC.S.005))
  ) %>% 
  dplyr::select(
    time.start, fNH3.flux, mD.cNH3, fNH3.ff1, fNH3.ff2, NEE.gf, u.star,
    BM.RH.382, BM.WD.382, LAI = LAI.gf, SL.SWC.005, BM.Tair.382, BM.R.lw.out
  ) %>% 
  mutate(
    # Calculate VPD
    BM.VPD.382 = calcVPD((BM.Tair.382 + 273.15), BM.RH.382), 
    # Approximate surface temperature with Stefan Boltzmann
    BM.Tsurf = (BM.R.lw.out/(5.67 * 10^-8))^0.25,
    # Discretize wind direction
    WD.sector = cut(x = BM.WD.382, 
                    breaks = seq(0,360,10),
                    labels = FALSE),
    # Get c_ave_prev
    cNH3.aveprev = calc_c_ave_prev(cNH3 = mD.cNH3, cSO2 = NA, 
                                   ndays = 6, interval = 1800),
    # Filter flux for two different ustar thresholds
    low = ifelse(fNH3.ff1 == TRUE, 
                 fNH3.flux, 
                 NA),
    high = ifelse(fNH3.ff2 == TRUE, 
                  fNH3.flux, 
                  NA)
  ) %>% 
  dplyr::select(- c(BM.WD.382, fNH3.ff1, fNH3.ff2, fNH3.flux, BM.Tair.382, 
                    BM.R.lw.out)) %>% 
  pivot_longer(
    cols = c(low, high),
    names_to = 'Threshold',
    values_to = 'fNH3.flux'
  )

# -- Separate training and testing dataset (keep March and July for training 
# purposes)
df_xg_fNH3_train <- df_xg_fNH3 %>%
  dplyr::filter(!format(time.start, "%m") %in% c("03", "07")) %>% 
  drop_na() 

df_xg_fNH3_test <- df_xg_fNH3 %>%
  dplyr::filter(format(time.start, "%m") %in% c("03", "07")) %>% 
  drop_na() 

# Train model with high u.star threshold ----
# -- set training data
df_reg_xg_h <- df_xg_fNH3_train %>% 
  dplyr::filter(Threshold == 'high') %>% 
  select(- Threshold)

# -- Define hyper-parameter settings and grid search
# use index to specify the rows for each fold
folds <- df_reg_xg_h %>%
  mutate(rn = row_number()) %>%
  mutate(fold = sample(1:3, n(), replace = TRUE)) %>%
  select(fold, rn) %>%
  group_by(fold) %>%
  reframe(l = list(rn = rn)) %>%
  select(-fold) %>%
  as.list()
names(folds$l) <- 1:3

# start the train control with 5 fold cross validation.
cv_xgboost <- trainControl(
  index = folds$l,
  method = "cv", # "cv" for k-fold cross-validation
  number = 3, # (we have also specified this in the fold index)
  verboseIter = TRUE, # Print progress
  allowParallel = TRUE,
  seeds = set.seed(42)
)

tr_tes_xgboost <- trainControl(
  index = folds$l,
  method = "LGOCV", p = 0.8, number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE, # Print progress
  allowParallel = TRUE,
  seeds = set.seed(42)
)

# set the parameters for the hyper parameter tuning
turn_grid_xgb <- expand.grid(
  eta = c(0.16),
  max_depth = c(10),
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1,
  nrounds = 100,
  gamma = c(0.05)
)

# tune and build the model using 3 fold cross validation
x_input <- as.data.frame(
  lapply(df_reg_xg_h[, !(names(df_reg_xg_h) %in% c("fNH3.flux", "time.start"))], 
         as.numeric),
  stringsAsFactors = FALSE)

model_xg_fNH3_h <- caret::train(
  x = x_input,
  y = df_reg_xg_h$fNH3.flux,
  method = "xgbTree", 
  tuneGrid = turn_grid_xgb,
  trControl = cv_xgboost,
  verbosity = 0
)

# Train model with low u.star threshold ----
# -- Set training data
df_reg_xg_l <- df_xg_fNH3_train %>% 
  dplyr::filter(Threshold == 'low') %>% 
  select(- Threshold)

# -- Define hyper-parameter settings and grid search
# use index to specify the rows for each fold
folds <- df_reg_xg_l %>%
  mutate(rn = row_number()) %>%
  mutate(fold = sample(1:3, n(), replace = TRUE)) %>%
  select(fold, rn) %>%
  group_by(fold) %>%
  reframe(l = list(rn = rn)) %>%
  select(-fold) %>%
  as.list()
names(folds$l) <- 1:3

# start the train control with 5 fold cross validation.
cv_xgboost <- trainControl(
  index = folds$l,
  method = "cv", # "cv" for k-fold cross-validation
  number = 3, # (we have also specified this in the fold index)
  verboseIter = TRUE, # Print progress
  allowParallel = TRUE,
  seeds = set.seed(42)
)

tr_tes_xgboost <- trainControl(
  index = folds$l,
  method = "LGOCV", p = 0.8, number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE, # Print progress
  allowParallel = TRUE,
  seeds = set.seed(42)
)

# set the parameters for the hyper parameter tuning
turn_grid_xgb <- expand.grid(
  eta = c(0.16),
  max_depth = c(10),
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1,
  nrounds = 100,
  gamma = c(0.05)
)

# tune and build the model using 3 fold cross validation
model_xg_fNH3_l <- caret::train(
  x = df_reg_xg_l %>% select(-c(fNH3.flux, time.start)),
  y = df_reg_xg_l$fNH3.flux,
  method = "xgbTree", # xgboost algorithm
  tuneGrid = turn_grid_xgb,
  trControl = cv_xgboost,
  verbosity = 0
)

# Apply models on all available data ----
df_reg_xg <- df_xg_fNH3_train %>% 
  mutate(fNH3.xg = if_else(
    condition = Threshold == 'low',
    true = predict(model_xg_fNH3_l, df_xg_fNH3_train),
    false = if_else(
      condition = Threshold == 'high',
      true = predict(model_xg_fNH3_h, df_xg_fNH3_train),
      false = NA
    )
  ))

# Test XGB model ----
# Test model on remaining data
df_xg_fNH3_test <- df_xg_fNH3_test %>% 
  mutate(fNH3.xg = if_else(
    condition = Threshold == 'low',
    true = predict(model_xg_fNH3_l, .),
    false = if_else(
      condition = Threshold == 'high',
      true = predict(model_xg_fNH3_h, .),
      false = NA)),
    Threshold = factor(Threshold, levels = c('low', 'high'))) %>% 
  select(time.start, Threshold, fNH3.flux, fNH3.xg)

# Get comparative statistics stats
df_xg_fNH3_test %>%
  group_by(Threshold) %>% 
  mutate(res = fNH3.flux - fNH3.xg) %>%
  summarise(
    rmse = sqrt(mean(res^2)),
    NSE = 1 - sum(res^2) / sum((fNH3.flux - mean(fNH3.flux))^2),
  ) %>% 
  knitr::kable(
    digits = 2, format = "simple", 
    caption = "Statistical comparison of XGB model resuts for high and low
    u.star thresholds against observed NH3-flux",
    booktabs = TRUE
  )

# Apply XGB model to gapfill data ----
dat_fNH3_gf <- dat_Loobos_30 %>% 
  mutate(
    # Calculate average SWC at 5cm depth
    SL.SWC.005 = rowMeans(select(., SL.SWC.W.005, SL.SWC.N.005, SL.SWC.E.005, SL.SWC.S.005))
  ) %>% 
  dplyr::select(
    time.start, fNH3.flux, fNH3.ff1, fNH3.ff2, mD.cNH3, HT.cNH3, NEE.gf, u.star,
    BM.RH.382, BM.WD.382, LAI = LAI.gf, SL.SWC.005, BM.Tair.382, BM.R.lw.out
    ) %>% 
  mutate(
    # Calculate VPD
    BM.VPD.382 = calcVPD((BM.Tair.382 + 273.15), BM.RH.382), 
    # Approximate surface temperature with Stefan Boltzmann
    BM.Tsurf = (BM.R.lw.out/(5.67 * 10^-8))^0.25,
    # Discretize wind direction
    WD.sector = cut(x = BM.WD.382, 
                    breaks = seq(0,360,10),
                    labels = FALSE),
    # Get c_ave_prev
    cNH3.aveprev = calc_c_ave_prev(cNH3 = mD.cNH3, cSO2 = NA, 
                                   ndays = 6, interval = 1800)) %>% 
  # Remove unnecessary vars
  select(- c(BM.WD.382, BM.Tair.382, BM.R.lw.out)) %>% 
  # Fill missing mD data with HT data
  mutate(
    mD.cNH3 = if_else(
      condition = is.na(mD.cNH3), 
      true = HT.cNH3 , 
      false = mD.cNH3
    ),
    HT.cNH3 = if_else(
      condition = is.na(HT.cNH3), 
      true = mD.cNH3 , 
      false = HT.cNH3
    ),
    # Impute remaining missing cNH3 data
    mD.cNH3 = na_interpolation(mD.cNH3, option = "linear"), 
    # Impute u.star and SL.SWC.005 for XGB model
    u.star = na_interpolation(u.star, option = "linear"), 
    SL.SWC.005 = na_interpolation(SL.SWC.005, option = "linear"),
    # Apply XGB model
    fNH3.xgb.l = predict(model_xg_fNH3_l, .),
    fNH3.xgb.h = predict(model_xg_fNH3_h, .), 
    # Gap-fill NH3-flux data per XGB-model
    fNH3.gf.ff1 = if_else(
      condition = fNH3.ff1 == TRUE & !is.na(fNH3.flux), 
      true = fNH3.flux, 
      false = fNH3.xgb.l
    ),
    fNH3.gf.ff2 = if_else(
      condition = fNH3.ff1 == TRUE & !is.na(fNH3.flux), 
      true = fNH3.flux, 
      false = fNH3.xgb.h)
    ) %>% 
  # Average XGB-model and calculate expected error due to gap-filling
  rowwise() %>%
  mutate(
    fNH3.flux.gf = mean(c_across(all_of(c('fNH3.gf.ff1', 'fNH3.gf.ff2')))),
    fNH3.flux.gf.std = sd(c_across(all_of(c('fNH3.gf.ff1', 'fNH3.gf.ff2'))))
  ) %>%
  ungroup()
