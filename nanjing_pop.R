rm(list = ls())
pacman::p_load(tidyverse, geodata, elevatr, terra, sf, scales, rayshader)

# 1. Prepare Data ----
# Get Nanjing polygon
china_map = geodata::gadm(
  country = "CHN",
  level = 2,
  path = tempdir()
) |> st_as_sf()
crs(china_map)

nanjing_sf = china_map |>
  filter(NAME_2 == "Nanjing")
plot(st_geometry(nanjing_sf))

# Get DEM 
nanjing_dem = elevatr::get_elev_raster(
  locations = nanjing_sf,
  z = 10,
  clip = "locations"
) |> 
  rast()
plot(nanjing_dem)
nanjing_dem_exaggerate = nanjing_dem * 2

# hillshade
attributes = terra::terrain(
  nanjing_dem_exaggerate,
  v = c("slope", "aspect"),
  unit = "radians"
)

nanjing_hillshade = terra::shade(
  slope = attributes[[1]],
  aspect = attributes[[2]],
  angle = 45, direction = 270
)

plot(nanjing_hillshade, col = hcl.colors(100, palette = "Grays"))

# pop
pop_100m = rast("resources/chn_pop_2025_CN_100m_R2024B_v1.tif")

nanjing_pop = terra::resample(
  pop_100m, nanjing_hillshade,
  method = "bilinear"
)
plot(nanjing_pop)

hillshade_no_pop = terra::ifel(
  is.na(nanjing_pop), nanjing_hillshade, NA
)

# 2. ggplot ----
hillshade_df = as.data.frame(hillshade_no_pop,
                             xy = TRUE,
                             na.rm = TRUE)
colnames(hillshade_df)[3] = "shade"
nanjing_pop_df = as.data.frame(nanjing_pop,
                               xy = TRUE,
                               na.rm = TRUE)
colnames(nanjing_pop_df)[3] = "pop"
summary(nanjing_pop_df$pop)
nanjing_pop_df = nanjing_pop_df |>
  mutate(pop = ifelse(pop < .1, NA, pop))
summary(nanjing_pop_df$pop)

brks = c(1, 10, 1e2, 1e3)
gg_nj_pop = ggplot() +
  geom_raster(data = hillshade_df,
              aes(x = x, y = y, fill = shade)) +
  scale_fill_gradient(low = "grey90",
                      high = "grey30",
                      guide = "none") +
  ggnewscale::new_scale_fill() +
  geom_raster(data = nanjing_pop_df,
              aes(x = x, y = y, fill = pop)) +
  scale_fill_viridis_c(name = "Population",
                       option = "plasma",
                       begin = .2, end = 1,
                       trans = "log10", breaks = brks,
                       labels = scales::comma,
                       guide = guide_colorbar(
                         barwidth = unit(2, "mm"),
                         barheight = unit(20, "mm"),
                         ticks.colour = "grey10",
                         frame.colour = "grey10"
                       )) +
  theme_void() +
  theme(legend.position = c(.8, .2),
        legend.title = element_text(margin = margin(b = 10)),
        legend.background = element_rect(fill = scales::alpha("white", .5), color = NA),
        legend.margin = margin(5,5,5,5)) +
  coord_sf(expand = FALSE)

rayshader::plot_gg(
  ggobj = gg_nj_pop,
  multicore = TRUE,
  width = 5,
  height = 10,
  scale = 30,
  solid = FALSE,
  shadow = TRUE,
  shadow_darkness = .6,
  sunangle = 270,
  windowsize = c(500, 1e3),
  zoom = .5,
  theta = 0,
  phi = 80
)  
rayshader::render_camera(
  zoom = .5,
  theta = -30,
  phi = 30
)
try(rgl::rgl.close(), silent = TRUE)
