rm(list = ls())
remotes::install_github("inSileco/rchelsa")
remotes::install_github("chris-prener/biscale")
pacman::p_load(sf, terra, tidyverse, elevatr, ggnewscale,
               rchelsa, biscale, cowplot, gridGraphics)

# CHELSA DATA ----
# define a vector of IDs to download
ids = c(1, 12)
download_chelsa_data = function(id){
  rchelsa::get_chelsea_data(
    categ = "clim",
    type = "bio",
    id = id,
    path = "resources/CHELSA/"
  )
}

lapply(ids, download_chelsa_data)
list.files("resources/CHELSA/")

# load the raster files
temp = terra::rast("resources/CHELSA/CHELSA_bio10_01.tif")
prec = terra::rast("resources/CHELSA/CHELSA_bio10_12.tif")

# average precipitation (1981-2010)
prec_average = prec / 30
plot(temp)
# Combine average temperature and precipitation into a raster stack
temp_prec = c(temp, prec_average)
head(temp_prec)
# assign names to each layer in the stack
names(temp_prec) = c("temp", "prec")

# POLYGON ----
clp_sf = read_sf("resources/LoessPlateauRegion/LoessPlateauRegion.shp")
clp_sf = st_transform(clp_sf, crs = "EPSG:4326")
# CROP AND RESAMPLE ----
target_crs = "EPSG:4610"

clp_temp_prec = terra::crop(
  temp_prec, clp_sf,
  mask = TRUE
)
# clp_temp_prec = project(clp_temp_prec, target_crs)
plot(clp_temp_prec)

# obtain DEM
clp_dem = elevatr::get_elev_raster(
  locations = clp_sf,
  z = 7,
  clip = "locations"
) |>
  terra::rast() |>
  terra::crop(clp_sf, mask = TRUE)

# resample the climate raster to match DEM resolution
clp_temp_prec_resampled = terra::resample(
  x = clp_temp_prec,
  y = clp_dem,
  method = "bilinear"
) |>
  terra::project(target_crs)

plot(clp_temp_prec_resampled)

# convert the raster to data frame with coordinates
clp_temp_prec_df = as.data.frame(
  clp_temp_prec_resampled,
  xy = TRUE,
  na.rm = TRUE
)

# BREAKS, PALETTE, AND PLOT THEME ----
# create bivariate classes using biscale
breaks = biscale::bi_class(
  clp_temp_prec_df,
  x = temp, y = prec,
  style = "fisher",
  dim = 3
)

pal = "GrPink"

theme = theme(
  axis.title = element_blank(),
  plot.background = element_rect(fill = "white", color = NA),
  plot.title = element_text(color = "grey10", face = "bold",
                            hjust = .5, vjust = -1),
  plot.subtitle = element_text(hjust = .5, vjust = -1),
  plot.caption = element_text(hjust = .5, vjust = -1),
  plot.margin = unit(c(0,0,0,0), "lines")
)

# HILLSHADE ----
# clp_dem_exaggerated = clp_dem * 2
# attributes = terra::terrain(
#   clp_dem_exaggerated,
#   v = c("slope", "aspect"),
#   unit = "radians"
# )
# hillshade = terra::shade(
#   attributes$slope,
#   attributes$aspect,
#   angle = 40, direction = 270
# )
# hillshade = project(hillshade, target_crs)
# plot(hillshade, col = grey(1:100/100))
# 
# hillshade_df = as.data.frame(
#   hillshade,
#   xy = TRUE,
#   na.rm = TRUE
# )

# BIVARIATE MAP ----
ggplot() +
  # geom_raster(data = hillshade_df,
  #             aes(x = x, y = y, fill = hillshade),
  #             show.legend = FALSE) +
  # scale_fill_gradient(low = "grey80",
  #                     high = "grey20") +
  # ggnewscale::new_scale_fill() +
  geom_raster(data = breaks,
              aes(x = x, y = y, fill = bi_class),
              show.legend = FALSE) +
  biscale::bi_scale_fill(
    pal = pal, dim = 3,
    flip_axes = FALSE, rotate_pal = FALSE
  ) +
  labs(
    title = "CLP: Temperature and Precipitation",
    subtitle = "Average temperature and precipitation (1981-2010)",
    caption = "Source: CHELSA | Author: Jiawei Da",
    x = "", y = "") +
  coord_sf(crs = target_crs) +
  theme_minimal() + theme

# create legend for the map
legend = biscale::bi_legend(
  pal = pal,
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 3,
  xlab = "Temperature (°C)",
  ylab = "Precipitation (mm)",
  size = 8
)

cowplot::ggdraw() +
  cowplot::draw_plot(
    plot = map, x = 0, y = 0,
    width = 1, height = 1
  ) +
  cowplot::draw_plot(
    plot = legend, x = .1, y = .55,
    width = .25, height = .25
  )
ggsave("figures/clp_bivariate_2d.png",
       width = 7, height = 5.7,
       dpi = 600, bg = "white")
















