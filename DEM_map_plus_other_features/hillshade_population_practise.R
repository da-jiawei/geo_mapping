rm(list = ls())
pacman::p_load(sf, terra, geodata, elevatr, tidyverse, ggspatial, ggnewscale, scales)

# shape boundary ----
twn_sf = geodata::gadm(
  country = "TWN", level = 0,
  path = tempdir()
) |>
  sf::st_as_sf()
plot(st_geometry(twn_sf))
twn_sf$geometry
twn_sf_main = twn_sf |>
  mutate(geometry = {
    polys = st_cast(geometry, "POLYGON")
    areas = st_area(polys)
    polys[which.max(areas)]
  }) |>
  ungroup()
plot(st_geometry(twn_sf_main))

# population ----
pop_100m = terra::rast(
  "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2024B/2020/TWN/v1/100m/constrained/twn_pop_2020_CN_100m_R2024B_v1.tif"
)
plot(pop_100m)
pop_twn = terra::crop(
  pop_100m, twn_sf_main
)
plot(pop_twn)

# dem ----
twn_dem = elevatr::get_elev_raster(
  locations = twn_sf_main,
  z = 10,
  clip = "locations"
) |>
  terra::rast()
plot(twn_dem)

twn_dem_exaggerated = twn_dem * 1.3

attributes = terra::terrain(
  twn_dem_exaggerated,
  v = c("slope", "aspect"),
  unit = "radians"
)

hillshade_raw = terra::shade(
  attributes$slope,
  attributes$aspect,
  angle = 40, direction = 270
)
plot(hillshade_raw, col = hcl.colors(100, palette = "Grays"))

pop_on_hillshade = terra::resample(
  pop_twn, hillshade_raw,
  method = "bilinear"
)

hillshade_no_pop = terra::ifel(
  is.na(pop_on_hillshade), hillshade_raw,
  NA
)

# data frame for ggplot
hillshade_no_pop = terra::project(hillshade_no_pop, "EPSG:3829")
pop_on_hillshade = terra::project(pop_on_hillshade, "EPSG:3829")
twn_sf_main = twn_sf_main |> st_transform(crs = "EPSG:3829")

hillshade_df = terra::as.data.frame(
  hillshade_no_pop,
  xy = TRUE,
  na.rm = TRUE
)

pop_df = as.data.frame(
  pop_on_hillshade,
  xy = TRUE,
  na.rm = TRUE
)

summary(pop_df$twn_pop_2020_CN_100m_R2024B_v1)
pop_df$twn_pop_2020_CN_100m_R2024B_v1[pop_df$twn_pop_2020_CN_100m_R2024B_v1 <= 0.1] = NA
summary(pop_df$twn_pop_2020_CN_100m_R2024B_v1)

# plot
brks = c(1, 10, 100, 1e3)
ggplot() +
  geom_raster(data = hillshade_df,
              aes(x, y, fill = twn_pop_2020_CN_100m_R2024B_v1)) +
  scale_fill_gradient(
    low = "grey90", high = "grey30",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = pop_df,
              aes(x, y, fill = twn_pop_2020_CN_100m_R2024B_v1)) +
  scale_fill_viridis_c(
    name = "Population",
    option = "plasma",
    begin = .2, end = 1,
    trans = "log10", breaks = brks,
    labels = scales::comma,
    guide = guide_colorbar(
      barwidth = unit(2, "mm"),
      barheight = unit(20, "mm"),
      ticks.colour = "grey10",
      frame.colour = "grey10",
      title.position = "top"
    )
  ) +
  geom_sf(data = twn_sf_main,
          color = "black",
          fill = NA,
          linewidth = .25) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(10, "mm"),
    width = unit(10, "mm"),
    style = north_arrow_orienteering()
  ) +
  ggspatial::annotation_scale(
    location = "br",
    pad_y = unit(2, "mm"),
    height = unit(2, "mm"),
    style = "ticks"
  ) +
  labs(title = "Taiwan · Population (2020)",
       subtitle = "WorldPop 100m constrained grid",
       caption = "Data: WorldPop · SRTM via elevatr | Design: Jiawei Da") +
  theme_void() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = .02),
        plot.subtitle = element_text(size = 14, hjust = .02),
        plot.caption = element_text(hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)) +
  coord_sf(expand = FALSE) 

ggsave("figures/taiwan_population_relief.png", width = 4, height = 5.4, dpi = 600, 
       bg = "white")








