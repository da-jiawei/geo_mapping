rm(list = ls())
pacman::p_load(sf, terra, geodata, elevatr, rnaturalearth, tidyverse, tidyterra, ggnewscale)

# download sf file
china_sf = rnaturalearth::ne_countries(
  scale = 'medium',
  country = c("china", "taiwan"),
  returnclass = "sf"
)
plot(st_geometry(china_sf))
crs(china_sf)

# download DEM
china_dem = elevatr::get_elev_raster(
  locations = china_sf,
  z = 5,
  clip = "locations"
)
china_dem = terra::rast(china_dem)
tidyterra::autoplot(china_dem)
china_dem = terra::project(china_dem, "EPSG:4480")
china_sf = china_sf |> st_transform(crs = "EPSG:4480")

# generate hillshade
china_attributes = terra::terrain(
  china_dem,
  v = c("slope", "aspect"),
  unit = "radians"
)
terra::plot(china_attributes$slope)
terra::plot(china_attributes$aspect)

hillshade = terra::shade(
  slope = china_attributes$slope,
  aspect = china_attributes$aspect,
  angle = 30, direction = 270
)
terra::plot(
  hillshade,
  col = grey(0:100 / 100),
  legend = FALSE,
  main = "Hillshade (Sun from West)"
)

# plot
china_dem_df = terra::as.data.frame(
  china_dem,
  xy = TRUE,
  na.rm = TRUE
)
colnames(china_dem_df)[3] = "elev"

hillshade_df = terra::as.data.frame(
  hillshade,
  xy = TRUE,
  na.rm = TRUE
)

limits = range(china_dem_df$elev)
ggplot() +
  geom_raster(data = hillshade_df,
              aes(x = x, y = y, fill = hillshade),
              show.legend = FALSE) +
  scale_fill_gradientn(
    colors = hcl.colors(12, "Grays", rev = FALSE),
    na.value = NA # transparent for any missing cells
  ) +
  ggnewscale::new_scale_fill() +
  geom_raster(data = china_dem_df,
              aes(x = x, y = y, fill = elev),
              alpha = .5) +
  tidyterra::scale_fill_hypso_tint_c(palette = "dem_poster",
                                     limits = limits) +
  labs(title = "Shaded Reief Map of China",
       caption = paste(
         "Data: via elevtr",
         "Hillshade: Sun from West (270 deg)"),
       fill = "Elevation (m)") +
  coord_sf(crs = "EPSG:4480") +
  guides(fill = guide_colorbar(
    keywidth = unit(.3, units = "cm"),
    keyheight = unit(2, units = "cm"),
    override.aes = list(alpha = 1)
  )) +
  theme_void() +
  theme(legend.position = c(.9, .3),
        legend.title = element_text(margin = margin(b = 10)),
        legend.key = element_rect(colour = "black"),
        axis.title = element_blank())

# use Natural Earth Map ----
rm(list = ls())
china_sf = rnaturalearth::ne_countries(
  scale = "medium",
  country = c("china", "taiwan"),
  returnclass = "sf"
)
world_map = rast("resources/HYP_HR_SR_OB_DR/HYP_HR_SR_OB_DR.tif")
china_map = terra::crop(world_map, china_sf, mask = TRUE)
china_map = china_map |> project("EPSG:4480")
china_map_df = as.data.frame(china_map,
                             xy = TRUE,
                             na.rm = TRUE)
china_map_df$fill_col = rgb(
  red = china_map_df$HYP_HR_SR_OB_DR_1,
  green = china_map_df$HYP_HR_SR_OB_DR_2,
  blue = china_map_df$HYP_HR_SR_OB_DR_3,
  maxColorValue = 255
)
ggplot() +
  geom_raster(data = china_map_df,
              aes(x = x, y = y, fill = fill_col)) +
  scale_fill_identity() +
  theme_void() +
  theme(axis.title = element_blank())






