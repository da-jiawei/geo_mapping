# Create a basic static ggplot map suitable as a base for animation
# transition_time(): animate ggplot maps based on a time variable (like year)
# {frame_time}: Incorporate dynamic labels (like the current year) into animated plot titles using gganimate variables 
# ease_aes(): Apply transitions and easing functions for smoother animation pacing
# anim_save(): save animations as GIF or video files

rm(list = ls())
# 1. Load packages -----
pacman::p_load(gganimate, sf, tidyverse, 
               gapminder, # Our time series dataset
               rnaturalearth, countrycode, # For matching country names to ISO codes
               gifski, # For rendering GIFs
               av # For rendering MP4s (needs ffmpeg)
               )

# 2. Prepare Data: Spatial + Time Series ----
# get world polygon
world_polygons_anim = rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  select(iso_a3 = adm0_a3, name, geometry) |>
  filter(name != "Antarctica")
plot(st_geometry(world_polygons_anim))

# get gapminder data and add matching ISO codes
gapminder_data_anim = gapminder::gapminder |>
  mutate(iso_a3 = suppressWarnings(
    countrycode(country, origin = 'country.name', destination = 'iso3c')
  )) |>
  select(!(continent)) |>
  filter(!is.na(iso_a3)) # remove rows with no matching country code 

# join spatial polygons with time series data
world_gapminder_sf = world_polygons_anim |>
  left_join(gapminder_data_anim, by = 'iso_a3')
glimpse(world_gapminder_sf)
# check for NAs in the joined columns
nrow(world_gapminder_sf)
n_distinct(world_gapminder_sf$iso_a3)
n_distinct(world_gapminder_sf$year)

# 3. Build the Basic static ggplot map ----
base_life_exp_map = ggplot(data = world_gapminder_sf) +
  geom_sf(aes(fill = lifeExp),
          color = "white",
          linewidth = .1) +
  scale_fill_viridis_c(option = "magma",
                       name = "Life Exp.",
                       na.value = "lightgrey") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, units = "cm"),
        plot.title = element_text(hjust = .5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = .5, size = 14))
base_life_exp_map

# 4. Adding Animation with transition_time() ----
animated_map = base_life_exp_map +
  transition_time(time = year) +
  labs(title = "Life Expectancy in Year: {as.integer(frame_time)}")
animated_map

# Render the animation (adjust nframes, fps for speed/smoothness)
anim_render = gganimate::animate(
  animated_map, nframes = 150, fps = 10, # lower values for faster previews
  width = 800, height = 500,
  renderer = gifski_renderer() # for reliable preview
)
anim_render

# 5. Smoothing Transition ----
animated_map_eased = base_life_exp_map +
  gganimate::transition_time(year) +
  labs(title = "Life Expectancy in Year: {as.integer(frame_time)}") +
  gganimate::ease_aes('sine-in-out')
anim_render_eased = gganimate::animate(
  animated_map_eased, nframes = 150, fps = 10,
  width = 800, height = 500,
  renderer = gifski_renderer()
)
anim_render_eased

# 6. Saving your animation ----
gganimate::anim_save(
  filename = "figures/life_expectancy.gif",
  animation = anim_render_eased,
  nframe = 200, 
  fps = 15,
  width = 800,
  height = 500,
  res = 100,
  renderer = gifski_renderer()
)
