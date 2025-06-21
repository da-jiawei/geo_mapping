rm(list = ls())
# Convert elevation raster data (from terra) into the matrix format required by rayshader
# Create a basic 3D terrain model using rayshader’s core functions (sphere_shade, plot_3d).
# Apply color or overlays to your 3D model using height_shade() or plot_gg() (draping ggplot maps!). 
# Adjust the camera angle (theta, phi) and lighting to control the view of your 3D scene. 
pacman::p_load(rayshader, terra, geodata, sf, tidyverse, magick)
# Xquartz, rgl

# Prepare data ----
country_iso = "CHE"
swiss_elev_raster = geodata::elevation_30s(
  country = country_iso, path = tempdir()
)
# convert SpatRaster to a matrix
elevation_matrix = rayshader::raster_to_matrix(
  swiss_elev_raster
)
dim(elevation_matrix)
elevation_matrix[1:5, 1:5]

# Create Simple 3D Terrain Models ----
# basic shading
elevation_matrix |>
  rayshader::sphere_shade(texture = "desert") |>
  rayshader::plot_3d(
    heightmap = elevation_matrix,
    zscale = 30, # Vertical Exaggeration
    solid = FALSE, # Make the base transparent
    fov = 0, # field of view
    theta = 0, # Rotation angle around Z axis
    phi = 80, # Vertical viewing angle
    zoom = .6, # zoom level 0-1
    windowsize = c(1000, 800),
    background = "lightgrey"
  )
# Sys.sleep(2)

# adding color based on elevation
elevation_matrix |>
  rayshader::height_shade(
    # create shading based on height
    texture = grDevices::terrain.colors(256)
  ) |>
  rayshader::plot_3d(
    heightmap = elevation_matrix,
    zscale = 10,
    solid = FALSE,
    fov = 0,
    theta = 0,
    phi = 80,
    zoom = .6,
    windowsize = c(1e3, 800),
    background = "lightgrey"
  )

# overlaying a ggplot2 map
names(swiss_elev_raster) = "elevation"
swiss_elev_raster_df = as.data.frame(
  swiss_elev_raster, xy = TRUE, na.rm = TRUE
)
# rayshader doesn't work well with theme_void()
# so we'll define our theme here
theme_for_the_win = function(){
  theme_minimal() +
    theme(axis.line = element_blank(),
          text = element_blank(), 
          panel.grid = element_blank(),
          plot.background = element_rect(
            fill = "white", color = NA
          ),
          legend.position = "none")
}

gg_swiss_elev = ggplot() +
  geom_raster(data = swiss_elev_raster_df,
              aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c(option = "mako", name = "") +
  theme_for_the_win()
gg_swiss_elev

rayshader::plot_gg(
  ggobj = gg_swiss_elev,
  multicore = TRUE,
  width = 7,
  height = 7, # width/height for texture
  scale = 30, # vertical exaggeration
  solid = FALSE,
  shadow = TRUE,
  shadow_darkness = .6,
  sunangle = 270,
  windowsize = c(1e3, 800),
  zoom = .5,
  theta = 0,
  phi = 80
)
Sys.sleep(2)

rayshader::render_camera(
  zoom = .6,
  theta = 0,
  phi = 30
)

# Saving your 3D map ----
# ensure the plot_gg window is open
rayshader::render_snapshot(
  file = "figures/swiss_3d_snapshot.png",
  title_text = "Swiss Alps (Snapshot)",
  title_bar_color = "black",
  title_color = "white",
  title_font = "sans",
  vignette = .2 # adds subtle darkening at edges
 )
# render a high quality image
rayshader::render_highquality(
  file = "figures/swiss_3d_highquality.png",
  light = TRUE,
  lightdirection = 315,
  lightintensity = 800,
  lightaltitude = 45,
  interactive = FALSE, # prevent extra preview windows during render
  width = 1200,
  height = 1e3,
  title_text = "Swiss Alps (High Quality Render)",
  title_offset = c(20, 20),
  title_color = "white",
  title_bar_color = "black"
)
try(rgl::rgl.close(), silent = TRUE)




