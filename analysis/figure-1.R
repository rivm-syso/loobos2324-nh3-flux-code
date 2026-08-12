##########'
#' figure-1
##'
#' Author: E.A.Melman @RIVM
#' Contributions by: 
##'
#' Usage:
##'
#' Aim: Plot maps of surroundings and  flux footprint. 
#' *NOTE*: For panel (c), the surface elevation map needs to be downloaded first. 
#' Instructions are given at the code block for panel (c).
#########' 
# Set coordintates observation site ----
sites_coordinates_rd <- data.frame(
  lat  = c(Loo$latitude),
  lon  = c(Loo$longitude),
  site = c("Loobos")
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(28992)

# Tower center location (Loobos) as numeric RD New coordinates
center_lon <- st_coordinates(sites_coordinates_rd)[1, 1]
center_lat <- st_coordinates(sites_coordinates_rd)[1, 2]

# Define target area ----
poly_coords <- matrix(c(
  5.737953186035157,52.14718401605892,
  5.711259841918945,52.160990957265724,
  5.707311630249023,52.1656241269461,
  5.759239196777345,52.19056190201232,
  5.7607412338256845,52.18104206548279,
  5.762028694152833,52.17119941495388,
  5.756278038024902,52.167672373831294,
  5.757565498352051,52.16461888856322,
  5.759024620056152,52.16251291456938,
  5.766792297363281,52.15371445951162,
  5.737953186035157,52.14718401605892 # close polygon
), byrow=TRUE,ncol=2)

poly_sf <- st_sf(
  geometry = st_sfc(
    st_polygon(list(poly_coords)),
    crs=4326
  )
)

poly_sf_rd <- st_transform(poly_sf, 28992)

# Panel (a), OSM map for land use ---
# -- Extent: ±20 km from tower
p_a <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 10) +
  geom_sf(
    data = sites_coordinates_rd,
    aes(color = site, shape = site), size = 4
  ) +
  geom_text(
    data = data.frame(x = center_lon - 19000, y = center_lat + 19000, label = "(a)"),
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 1, size = 3.5
  ) +
  scale_color_manual(values = c("red", "black"), 
                     breaks = c("Loobos"), 
                     labels = c("Loobos"), 
                     name = NULL) +
  scale_shape_manual(values = c(17, 15),         
                     breaks = c("Loobos"), 
                     labels = c("Loobos"), 
                     name = NULL) +
  scale_x_continuous(
    breaks = seq(center_lon - 20000, center_lon + 20000, by = 10000),
    labels = function(x) (x - center_lon)/1000
  ) +
  scale_y_continuous(
    breaks = seq(center_lat - 20000, center_lat + 20000, by = 10000),
    labels = function(y) (y - center_lat)/1000
  ) +
  coord_sf(
    crs    = 28992,
    xlim   = c(center_lon - 20000, center_lon + 20000),
    ylim   = c(center_lat - 20000, center_lat + 20000),
    expand = FALSE,
    datum  = 28992
  ) +
  labs(x = "Distance east (km)", y = "Distance north (km)", shape = "Site", color = "Site") +
  theme(legend.position = "bottom",
        legend.background     = element_blank(),
        legend.key.size       = unit(0.4, "cm"),
        legend.key.spacing.x  = unit(0.05, "cm"),
        legend.key.spacing.y  = unit(0.01, "cm"),
        legend.spacing.x      = unit(-0.5, "cm"),
        legend.box.spacing = unit(0, "pt"))

# Panel (b): Satellite image ----
# -- Extent: ±1000 m from tower
xmin <- as.numeric(center_lon) - 3000
xmax <- as.numeric(center_lon) + 3000
ymin <- as.numeric(center_lat) - 3000
ymax <- as.numeric(center_lat) + 3000

# -- Create extent object
loobos_extent <- c(xmin, xmax, ymin, ymax)

# -- Create bbox as an sf object
loobos_bbox_rd <- st_bbox(c(
  xmin = loobos_extent[1],
  ymin = loobos_extent[3],
  xmax = loobos_extent[2],
  ymax = loobos_extent[4]
), crs = 28992)

# -- Calculate footprint
dat_cleaned <- dat_Loobos_30 %>%
  dplyr::filter(
    !is.na(BM.WS.382), !is.na(h.abl), !is.na(L),
    !is.na(v.var), !is.na(u.star), !is.na(BM.WD.382),
    h.abl  > 40,
    fNH3.ff2 == TRUE,
    !is.na(fNH3.flux)
  )

FFP <- calc_footprint_FFP_climatology(
  zm       = 38.2 - Loo$d,
  z0       = Loo$z0,
  umean    = dat_cleaned$BM.WS.382,
  h        = dat_cleaned$h.abl,
  ol       = dat_cleaned$L,
  sigmav   = dat_cleaned$v.var,
  ustar    = dat_cleaned$u.star,
  domain   = c(-3000,3000,-3000,3000),
  wind_dir = dat_cleaned$BM.WD.382,
  r        = seq(50, 80, 10),
  rslayer  = 1
)

# Build contour data frame (skip NA levels)
r_levels  <- seq(50, 80, 10)
valid_idx <- seq(1,4)

contour_df <- do.call(rbind, lapply(valid_idx, function(i) {
  data.frame(
    x     = c(FFP$xr[[i]], FFP$xr[[i]][1]),  # close polygon
    y     = c(FFP$yr[[i]], FFP$yr[[i]][1]),
    level = paste0(r_levels[i], "%"),
    r     = r_levels[i]
  )
}))

# Outermost level first so inner polygons paint on top
contour_df$level <- factor(
  contour_df$level,
  levels = paste0(sort(r_levels[valid_idx], decreasing = TRUE), "%")
)
n_levels <- length(valid_idx)
fill_pal  <- brewer.pal(max(3, n_levels), "YlOrRd")[seq_len(n_levels)]

# -- Download satellite tiles
sat_tiles <- get_tiles(
  st_as_sfc(loobos_bbox_rd),
  provider = "Esri.WorldImagery",
  zoom = 15,
  crop = TRUE
)

# -- Plot map
p_b <- ggplot() +
  geom_spatraster_rgb(data = sat_tiles) +
  geom_sf(
    data=poly_sf,
    color='grey75',
    linewidth = 1,
    alpha=.0,
  ) +
  geom_hline(yintercept = center_lat, color = "white", linewidth = 0.3) +
  geom_vline(xintercept = center_lon, color = "white", linewidth = 0.3) +
  geom_polygon(data = contour_df,
               aes(x = x + center_lon, 
                   y = y + center_lat, 
                   fill = level, 
                   group = level), 
               alpha = 0.6, colour = NA) +
  geom_path(data = contour_df,
            aes(x = x + center_lon, 
                y = y + center_lat, 
                colour = level, 
                group = level), 
            linewidth = 0.6) +
  geom_text(
    data = data.frame(x = center_lon - 2850, y = center_lat + 2850, label = "(b)"),
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 1, size = 3.5, color = "white"
  ) +
  scale_x_continuous(
    breaks = seq(center_lon - 3000, center_lon + 3000, by = 1000),
    labels = function(x) (x - center_lon)/1000
  ) +
  scale_y_continuous(
    breaks = seq(center_lat - 3000, center_lat + 3000, by = 1000),
    labels = function(y) (y - center_lat)/1000
  ) +
  coord_sf(
    crs    = 28992,
    xlim   = c(center_lon - 3000, center_lon + 3000),
    ylim   = c(center_lat - 3000, center_lat + 3000),
    expand = FALSE,
    datum  = 28992
  ) +
  scale_fill_manual(values = fill_pal, name = NULL) +
  scale_colour_manual(values = fill_pal, name = NULL) +
  guides(
    fill   = guide_legend(nrow = 1),
    colour = guide_legend(nrow = 1)
  ) +
  labs(x = "Distance east (km)", 
       y = NULL
  ) + 
  theme(
    legend.background     = element_blank(),
    legend.key.size       = unit(0.4, "cm"),
    legend.key.spacing.x  = unit(0.05, "cm"),
    legend.key.spacing.y  = unit(0.01, "cm"),
    legend.spacing.x      = unit(-0.5, "cm"),
    legend.box.spacing = unit(0, "pt")
  )

# Panel (c), surface elevation (AHN) ----
# -- Extent: ±500 m from tower
# NOTE: Download R_32FZ2 tile from www.ahn.nl to complete this figure
# ahn4_raster <- rast("your_path/R_32FZ2.TIF")
# crs(ahn4_raster) <- "EPSG:28992"
# ahn4_raster_crop <- crop(ahn4_raster, ext(
#   center_lon - 500, center_lon + 500,
#   center_lat - 500, center_lat + 500
# ))

# ahn4_df <- as.data.frame(ahn4_raster_crop, xy = TRUE)
# colnames(ahn4_df) <- c("x", "y", "elevation")
# ahn4_df$rel_x <- ahn4_df$x - center_lon
# ahn4_df$rel_y <- ahn4_df$y - center_lat

p_c <- ggplot() +
  # geom_raster(data = ahn4_df, aes(x = rel_x, y = rel_y, fill = elevation)) +
  geom_hline(yintercept = 0, color = "white", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "white", linewidth = 0.3) +
  annotate("point", x = 0, y = 0, color = "red", shape = 17, size = 4) +
  geom_text(
    data = data.frame(x = -490, y = 490, label = "(c)"),
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 1, size = 3.5, color = "white"
  ) +
  scale_fill_viridis(option = "magma") +
  # scale_x_continuous(breaks = seq(-500, 500, 250), 
  #                    limits = c(-500, 500)) +
  scale_y_continuous(
    breaks = c(-500,-300, -100,0,100,300,500),
    limits = c(-500, 501),
    labels = c(-.500,-.300,-0.1,0,0.1,.300,.500) 
  ) + 
  scale_x_continuous(
    breaks = seq(-500, 501, by = 250),
    limits = c(-500, 501),
    labels = seq(-.5, .501, by = .25) 
  ) + 
  coord_fixed(expand = FALSE) +
  labs(fill = NULL, x = "Distance east (km)", y = NULL) +
  guides(fill = guide_colourbar(ticks = TRUE)) +
  theme(
    legend.position        = "right",
    legend.title           = element_blank(),
    legend.title.position  = "top",
    # legend.margin          = margin(0, 0, 0, 0),
    # legend.box.margin      = margin(0, 0, 0, 0),
    legend.key.width       = unit(0.15, "cm"),
    legend.key.height      = unit(0.8, "cm"),
    axis.ticks             = element_line(),
    panel.grid.major       = element_line(color = "grey"),
    legend.box.spacing = unit(0, "pt")
  )

# Built figure and save fig ----
fig1 <- (p_a + p_b + p_c) + 
  plot_annotation() +
  plot_layout(axis_titles = "collect")

ggsave(
  fig1,
  file   = "Fig1.png",
  path   = fig_path,
  width  = 175,
  height = 0.8*wsingle,
  units  = "mm",
  dpi    = 600
)
ggsave(
  fig1,
  file   = "Fig1.pdf",
  path   = fig_path,
  width  = 175,
  height = 0.8*wsingle,
  units  = "mm",
  dpi    = 600
)
