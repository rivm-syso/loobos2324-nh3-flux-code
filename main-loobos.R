##########'
#' main-loobos
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage: figures will be stored in the folder "fig_path". Please set your 
#' desired output path inside the "Set project info" code block. 
##'
#' Aim: This script initializes analysis to NH3 flux data collected over ICOS 
#' Class 2 ecosystem station Loobos, a Scots pine forest in the Netherlands. The
#' accompanying scripts contain the code to reproduce figures and tables as 
#' presented in Melman et al. (2026).
#' *NOTE*: To run figure-B1.R and figure-B2.R, xgb-gapfilling.R must be ran first. 
#########'
# Install necessary packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  # General
  ggplot2, cowplot, dplyr, tidyr, patchwork, zoo, lubridate, RColorBrewer,  
  # main-loobos
  data.table, archive, httr,
  # figure-1
  sf, ggspatial, tidyterra, maptiles,
  # figure-5
  ggExtra, 
  # figure-6
  ggh4x,
  # figure-8
  ggpmisc,
  # XGB-gapfilling
  caret, xgboost, imputeTS, 
  # Update packages statement
  update = F)

# Set project info ----
# -- Set project paths
fig_path <- file.path(".", "figures/")
if (!dir.exists(fig_path)) dir.create(fig_path, recursive = TRUE)

# Set site info ----
Loo <- list()

# -- Set site coordinates
Loo$latitude <- 52.16649628
Loo$longitude <- 5.743581772

# -- Roughness and vegetation parameters
Loo$hc <- 21 # canopy height [m]
Loo$d <- 15.5 # displacement height [m]
Loo$z0 <- 1 # roughness length [m]

# -- Set campaign info
# Define campaign start and end
campaign_start <- as.POSIXct("2023-08-25 00:00:00",  tz = "UTC")
campaign_end <- as.POSIXct("2024-09-04 00:00:00", tz = "UTC")

# Define HT deployments
ht_deployment23_start <- campaign_start
ht_deployment23_end <- as.POSIXct('2023-11-09 08:00:00', tz = 'UTC')
ht_deployment24_start <- as.POSIXct('2024-02-09 13:30:00', tz = 'UTC')
ht_deployment24_end <- campaign_end

# Download data ----
#' *NOTE*: Check if a new version of data is available at zenodo repository (!)
# -- ZENODO url to data repository
dat_url <- "https://zenodo.org/record/20666694/files/Loobos_2023-2024_data.csv"

# -- Read data
dat_Loobos_30 <- fread(dat_url)

# Define functions ----
source(file.path(".", "functions/calc-c-ave-prev.R"))
source(file.path(".", "functions/calc-gamma.R"))
source(file.path(".", "functions/meteo-functions.R"))

# -- FFP Kljun et al., 2015
source(file.path(".", "functions/calc-footprint-ffp-climatology.R"))

# Set conversion factors ----
# -- Convert ug NH3-N per m2 to kg N-NH3 per ha
conv_ugNH3m2_to_kgNha <- 1e-9 * 1e4 * (14 / 17.031) # (ug/m2) * (1e-9 kg/ug) * (1e4 m2/ha) * (14 g N / 17.031 g NH3)

# -- Convert umol CO2 per m2 to metric tons C per ha
conv_umolCO2m2_to_tonCha <- 0.0001201 * 1e-3  # (umol/m2) * (0.0001201 ton C/umol) * (1e-3 ha/m2)

# -- Convert umol CO2 per m2 per second to grams C per m2 per second
conv_umolCO2m2s_to_gCm2s <- 12.01 * 1e-6  # (umol/m2/s) * (12.01 g/mol) * (1e-6 mol/umol)

# -- nr. seconds in half-hour for flux integration
conv_sec_to_halfhour <- 1800  # 1 half-hour = 1800 seconds

# Prepare analysis and plot settings ----
# -- Define figures size
wsingle <- 83 # mm, single column size,
wdouble <- 120 # mm, double column size
wfull <- 146 #mm, pagewidth size

# -- Set figures themes
theme_set(
  theme(
    text = element_text(size = 9),
    plot.title = element_text(size = 9),
    plot.tag = element_text(size = 9),
    legend.position = 'bottom',
    legend.key = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey25"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
)

# -- Load colors RIVM Housestyle
RIVM_colors = c(
  '#e17000', # 1 orange
  '#007bc7', # 2 Hemelblauw
  '#673327', # 3 Donkerbruin
  '#ca005d', # 4 Robijnrood
  '#777c00', # 5 Mosgroen
  '#42145f', # 6 Paars
  '#8fcae7', # 7 Lichtblauw
  '#ffb612', # 8 Donkergeel
  '#275937', # 9 Donkergroen
  '#cccccc', # 10 Grijd
  '#a90061', # 11 violet
  '#94710a' # 12 bruin
)

# -- Figure annotations
add_missing_cNH3 <- function() {
  list(
    annotate("rect", 
             xmin = as.POSIXct('2023-08-01 00:30:00', tz = 'UTC'),
             xmax = campaign_start,
             ymin = -Inf, ymax = Inf, fill = 'grey80'),
    annotate("rect", 
             xmin = campaign_end,
             xmax = as.POSIXct('2024-09-30 23:30:00', tz = 'UTC'),
             ymin = -Inf, ymax = Inf, fill = 'grey80')
  )
}

add_missing_fNH3 <- function() {
  list(
    annotate("rect", 
             xmin = as.POSIXct('2023-08-01 00:30:00', tz = 'UTC'),
             xmax = campaign_start,
             ymin = -Inf, ymax = Inf, fill = 'grey80'),
    annotate("rect", 
             xmin = ht_deployment23_end,
             xmax = ht_deployment24_start,
             ymin = -Inf, ymax = Inf, fill = 'grey80') ,
    annotate("rect", 
             xmin = campaign_end,
             xmax = as.POSIXct('2024-09-30 23:30:00', tz = 'UTC'),
             ymin = -Inf, ymax = Inf, fill = 'grey80')
  )
}

# Function for seasonal blocks (you supply ymin/ymax)
add_season_rects <- function(ymin, ymax) {
  list(
    annotate("rect", xmin = as.POSIXct('2023-08-01 00:00:00', tz = 'UTC'),
             xmax = as.POSIXct('2023-09-30 23:30:00', tz = 'UTC'),
             ymin = ymin, ymax = ymax, fill = RIVM_colors[1]),
    annotate("rect", xmin = as.POSIXct('2023-10-01 00:00:00', tz = 'UTC'),
             xmax = as.POSIXct('2024-02-29 23:30:00', tz = 'UTC'),
             ymin = ymin, ymax = ymax, fill = RIVM_colors[2]),
    annotate("rect", xmin = as.POSIXct('2024-03-01 00:00:00', tz = 'UTC'),
             xmax = as.POSIXct('2024-09-30 23:30:00', tz = 'UTC'),
             ymin = ymin, ymax = ymax, fill = RIVM_colors[1])
  )
}

