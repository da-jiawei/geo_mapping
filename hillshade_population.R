rm(list = ls())
pacman::p_load(terra, sf, elevatr, tidyverse, ggnewscale, ggspatial, geodata, scales)

# 1. swiss boundary
country_sf = geodata::gadm(
  country = "CHE", level = 0,
  path = tempdir()
) |> sf::st_as_sf()
plot(st_geometry(country_sf))
country_vect = terra::vect(country_sf)

# 2. Worldpop 100m population count 2020 ----
pop_100m = terra::rast(
  "https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/BSGM/CHE/che_ppp_2020_constrained.tif"
)
terra::plot(pop_100m)

# 3. DEM & hill-shade on finer (~30 m) grid ----
dem = elevatr::get_elev_raster(
  country_sf,
  z = 10,
  clip = "locations"
)

dem_country = terra::rast(dem) |>
  terra::crop(country_vect) |>
  terra::mask(country_vect)

# exaggerate the DEM ----
dem_exaggerated = dem_country * 1.3

# shaded relief ----
attributes = terra::terrain(
  dem_exaggerated,
  v = c("slope", "aspect"),
  unit = "radians"
)

hillshade_raw = terra::shade(
  attributes$slope,
  attributes$aspect,
  angle = 40, direction = 225
)
plot(hillshade_raw, col = hcl.colors(12, palette = "Grays"))

# 4. resample pop onto the hill-shade grid, ----
#    then blank relief where pop exists

pop_on_hillshade = terra::resample(
  pop_100m, hillshade_raw,
  method = "bilinear"
)

hillshade_no_pop = terra::ifel(
  is.na(pop_on_hillshade), hillshade_raw,
  NA
)

# 5. data frames fpr ggplot ----
hillshade_df = terra::as.data.frame(
  hillshade_no_pop,
  xy = TRUE,
  na.rm = TRUE
)

head(hillshade_df)

pop_df = terra::as.data.frame(
  pop_on_hillshade,
  xy = TRUE,
  na.rm = TRUE
)

head(pop_df)
summary(pop_df$che_ppp_2020_constrained)
pop_df$che_ppp_2020_constrained[pop_df$che_ppp_2020_constrained <= 0.1] = NA

# 6. plot ----
brks = c(1, 10, 100, 1e3)
p = ggplot() +
  geom_raster(data = hillshade_df, aes(
    x, y,
    fill = che_ppp_2020_constrained
  )) +
  scale_fill_gradient(
    low = "grey70", high = "grey10",
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = pop_df, aes(
    x, y,
    fill = che_ppp_2020_constrained
  )) +
  scale_fill_viridis_c(
    name = "Population",
    option = "plasma",
    alpha = 1, begin = .2, end = 1,
    trans = "log10", breaks = brks,
    labels = scales::comma,
    guide = guide_colorbar(
      title.position = "top",
      barheight = unit(30, "mm"),
      barwidth = unit(2, "mm"),
      ticks.color = "grey10",
      frame.color = "grey10"
    )
  ) +
  geom_sf(
    data = country_sf, fill = NA,
    color = "black", linewidth = .25
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(10, "mm"),
    width = unit(10, "mm"),
    style = north_arrow_orienteering()
  ) +
  annotation_scale(
    location = "br", pad_y = unit(2, "mm"),
    height = unit(2, "mm")
  ) +
  coord_sf(expand = FALSE) +
  labs(title = "Switzerland · Population (2020)",
       subtitle = "WorldPop 100m constrained grid",
       caption = "Data: WorldPop · SRTM via elevatr | Design: Milos Makes Maps") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = .02),
        plot.subtitle = element_text(size = 14, hjust = .02),
        plot.caption = element_text(hjust = .5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        legend.margin = margin(t = 0, r = 5, b = 0, l = 3),
        plot.margin = margin(5,5,5,5)) +
  theme_void()
 
ggsave(
  "figures/switzerland_population_relief.png",
  width = 8, height = 5, dpi = 600,
  bg = "white", p
)









