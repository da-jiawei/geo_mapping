rm(list = ls())
pacman::p_load(tidyverse, sf, terra, rchelsa, rnaturalearth, gganimate, gifski)
target_crs = "EPSG:3857"
# Prepare data ----
# region polygon
world_sf = rnaturalearth::ne_download(
  scale = 'medium',
  type = 'land',
  category = 'physical',
  returnclass = 'sf'
) |>
  st_transform(crs = target_crs)
crs(world_sf)
plot(st_geometry(world_sf))

east_asia_bbox = st_bbox(c(xmin = 100, xmax = 150, 
                              ymin = 20, ymax = 50), crs = st_crs(4326)) |>
  st_as_sfc() |>
  st_transform(crs = target_crs)

east_asia_sf = st_intersection(world_sf, east_asia_bbox)
plot(st_geometry(east_asia_sf))

# MAP data 
tif_files = list.files("resources/CHELSA/monthly_prec/",
                       pattern = "\\.tif$", full.names = TRUE)
prec_list = terra::rast(tif_files)
names(prec_list) = paste0("prec_", seq_along(tif_files))

# lower resolution
world_prec_stack = terra::aggregate(prec_list, fact = 10)
world_prec_stack = world_prec_stack |>
  terra::project(target_crs)
# crop raster
east_asia_prec = terra::crop(
  world_prec_stack, east_asia_sf, mask = TRUE
) |>
  terra::project("EPSG:4326")
plot(east_asia_prec)

# data frame for ggplot
east_asia_prec_df = as.data.frame(
  east_asia_prec, xy = TRUE, na.rm = TRUE
)
head(east_asia_prec_df)

east_asia_prec_df_long = east_asia_prec_df |>
  pivot_longer(cols = starts_with("prec_"),
               names_to = "month",
               values_to = "prec") |>
  mutate(month = as.integer(sub("prec_", "", month)))
head(east_asia_prec_df_long)

# plot map ----
base_map = ggplot() +
  geom_raster(data = east_asia_prec_df_long,
              aes(x = x, y = y, fill = prec)) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  guides(fill = guide_colorbar(
    title = "prec (mm)",
    barwidth = unit(3, units = "mm"),
    barheight = unit(20, units = "mm")
  )) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(.9, .25),
        legend.title = element_text(size = 12, margin = margin(b = 10)),
        legend.text = element_text(size = 10),
        legend.ticks = element_line(color = "black"),
        legend.frame = element_rect(color = "black"),
        legend.background = element_blank(),
        plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(labels = function(x)paste0(x, "°E")) +
  scale_y_continuous(labels = function(x)paste0(x, "°W")) +
  coord_sf(expand = FALSE)
base_map

anim_map = base_map +
  transition_time(time = month) +
  labs(title = "Month {as.integer(frame_time)}") +
  gganimate::ease_aes("sine-in-out")

anim_render = gganimate::animate(
  anim_map, nframes = 200, fps = 20,
  width = 600, height = 350,
  renderer = gifski_renderer()
)
anim_render

gganimate::anim_save(
  filename = "figures/east_asia_monthly_prec.gif",
  animation = anim_render)












