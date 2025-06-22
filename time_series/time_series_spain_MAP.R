rm(list = ls())
pacman::p_load(tidyverse, sf, terra, rnaturalearth, elevatr, gganimate, gifski)

# 1. Prepare Data ----
# spain polygon
spain_raw_sf = rnaturalearth::ne_countries(
  scale = 'large',
  country = 'spain',
  returnclass = 'sf'
) |>
  select(admin, geometry)
spain_raw_sf = st_cast(spain_raw_sf, "POLYGON") # parse out multipolygon
spain_raw_sf$area = st_area(spain_raw_sf)
max_area_index = which.max(spain_raw_sf$area)
spain_sf = spain_raw_sf[max_area_index,]
plot(st_geometry(spain_sf))
portugal_raw_sf = rnaturalearth::ne_countries(
  scale = 'large',
  country = 'portugal',
  returnclass = 'sf'
) |>
  select(admin, geometry)
portugal_raw_sf = st_cast(portugal_raw_sf, "POLYGON")
portugal_raw_sf$area = st_area(portugal_raw_sf)
max_area_index = which.max(portugal_raw_sf$area)
portugal_sf = portugal_raw_sf[max_area_index,]
plot(st_geometry(portugal_sf))
iberian_sf = rbind(spain_sf, portugal_sf)
plot(st_geometry(iberian_sf))

# spain dem 
iberian_dem = elevatr::get_elev_raster(
  locations = iberian_sf,
  z = 6,
  clip = "locations"
) |> rast()
plot(iberian_dem)
attributes = terra::terrain(
  iberian_dem,
  v = c("slope", "aspect"),
  unit = "radians"
)
hillshade = terra::shade(
  attributes$slope,
  attributes$aspect,
  angle = 45, direction = 270
)
hillshade_exaggerated = hillshade * 1.3
plot(hillshade_exaggerated, col = hcl.colors(12, "Grays"))

# spain MAP
file.names = list.files(path = "resources/CHELSA/monthly_prec/",
                        pattern = "\\.tif$",
                        full.names = TRUE)
MAP.stack = rast(file.names)
names(MAP.stack) = paste0("prec_", seq_along(file.names))

iberian_prec_stack = terra::crop(MAP.stack, iberian_sf, mask = TRUE)
iberian_prec_stack_low = terra::aggregate(iberian_prec_stack, fact = 10)
plot(iberian_prec_stack_low)

# 2. Plot Base Map ----
target_crs = "EPSG:2062"
iberian_sf = st_transform(iberian_sf, crs = target_crs)
hillshade = hillshade_exaggerated |>
  project(target_crs)
hillshade_df = as.data.frame(hillshade,
                             xy = TRUE, na.rm = TRUE)
iberian_prec_stack_low = iberian_prec_stack_low |>
  project(target_crs)
iberian_prec_stack_df = as.data.frame(iberian_prec_stack_low,
                                    xy = TRUE, na.rm = TRUE)
iberian_prec_df = iberian_prec_stack_df |>
  pivot_longer(cols = starts_with("prec_"),
               values_to = "prec",
               names_to = "month") |>
  mutate(month = as.integer(sub("prec_", "", month)))
range(iberian_prec_df$prec)

base_map = ggplot() +
  geom_raster(data = hillshade_df,
              aes(x = x, y = y, fill = hillshade)) +
  scale_fill_gradientn(
    colors = hcl.colors(12, "Grays", rev = FALSE),
    guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_raster(data = iberian_prec_df,
              aes(x = x, y = y, fill = prec),
              alpha = .5) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  coord_sf(crs = target_crs,
           expand = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.position = c(.9, .3),
        legend.title = element_text(size = 12, margin = margin(b = 10)),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 12, color = "black"),
        legend.ticks = element_line(colour = "black"),
        legend.frame = element_rect(color = "black"),
        axis.title = element_blank()) +
  labs(fill = "mm")
base_map

# 3. Animated Map ----
anim_map = base_map +
  gganimate::transition_time(time = month) +
  labs(title = "Month {as.integer(frame_time)}") +
  gganimate::ease_aes("sine-in-out")

anim_render = gganimate::animate(
  anim_map,
  nframes = 150, fps = 20,
  width = 600, height = 500,
  renderer = gifski_renderer()
)

gganimate::save_animation(
  animation = anim_render,
  file = "figures/iberian_time_series_prec.gif"
)













