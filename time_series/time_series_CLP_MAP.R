rm(list = ls())
pacman::p_load(tidyverse, sf, terra, gganimate, gifski, rnaturalearth)
default_crs = "EPSG:4326"

# Prepare data ----
# clp_polygon
clp_sf = read_sf("resources/LoessPlateauRegion/LoessPlateauRegion.shp")
target_crs = st_crs(clp_sf)
clp_sf_transform = st_transform(clp_sf, crs = default_crs)

# monthly precipitation
file_names = list.files(path = "resources/CHELSA/monthly_prec/",
                        pattern = "\\.tif$",
                        full.names = TRUE)
prec_stack = terra::rast(file_names)
names(prec_stack) = paste0("prec_", seq_along(file_names))
crs(prec_stack)

# crop 
clp_prec = terra::crop(
  prec_stack, clp_sf_transform, mask = TRUE
)
clp_prec_transform = clp_prec |>
  terra::aggregate(fact = 10) |>
  terra::project(target_crs$wkt)
plot(clp_prec_transform)

# data frame
clp_prec_df = as.data.frame(
  clp_prec_transform, xy = TRUE, na.rm = TRUE
)
clp_prec_df_new = clp_prec_df |>
  pivot_longer(cols = starts_with("prec_"),
               names_to = "month", 
               values_to = "prec") |>
  mutate(month = as.integer(sub("prec_", "", month))) |>
  filter(month > 3 & month < 11)

# ggplot
basemap = ggplot() +
  geom_raster(data = clp_prec_df_new,
              aes(x = x, y = y, fill = prec)) +
  # scale_fill_distiller(palette = "Blues", direction = 1) +
  scale_fill_viridis_c(direction = -1) +
  geom_sf(data = clp_sf, color = "grey20", fill = "transparent", linewidth = 2) +
  coord_sf(expand = FALSE) +
  theme_void() +
  guides(fill = guide_colorbar(
    title = "prec (mm)",
    barwidth = unit(5, "mm"),
    barheight = unit(30, "mm")
  )) +
  theme(legend.position = c(.2, .8),
        legend.title = element_text(size = 15, margin = margin(b = 10)),
        legend.text = element_text(size = 15),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"), 
        legend.ticks = element_line(color = "grey20"),
        legend.frame = element_rect(color = "grey20"))
basemap
clp_anim = basemap +
  gganimate::transition_time(time = month) +
  labs(title = "Month {as.integer(frame_time)}") +
  gganimate::ease_aes("sine-in-out")

clp_render = gganimate::animate(
  clp_anim,
  nframes = 100, fps = 20,
  width = 532, height = 483,
  renderer = gifski_renderer()
)
clp_render

gganimate::anim_save(
  filename = "figures/clp_monthly_prec.gif",
  animation = clp_render
)












