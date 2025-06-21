rm(list = ls())
pacman::p_load(leaflet, sf, tidyverse, giscoR, glue, htmlwidgets)
# giscoR helps get boundaries
# glue helps customize popups
# htmlwidgets helps save maps

# Empty Map with base tiles ----
basic_map = leaflet() |>
  addTiles()
basic_map

# Choosing your background: provider Tiles ----
provider_map_light = leaflet() |>
  addProviderTiles(
    # http://leaflet-extras.github.io/leaflet-providers/preview/
    provider = providers$CartoDB.Positron
  ) |>
  setView(lng = 0, lat = 30, zoom = 2)
provider_map_light

# Try another one
provider_map_sat = leaflet() |>
  addProviderTiles(
    provider = providers$Esri.WorldImagery
  ) |>
  setView(lng = 10, lat = 50, zoom = 4)
provider_map_sat

# Adding your data: Markers and Polygons ----
city_data_df <- data.frame(
  name = c("Amsterdam", "London", "Tokyo", "Nairobi", "Rio de Janeiro"),
  latitude = c(52.37, 51.51, 35.68, -1.29, -22.91), # Latitudes
  longitude = c(4.89, -0.13, 139.69, 36.82, -43.17), # Longitudes
  info = c("Canals & Culture",
           "Big Ben & Buses",
           "Bustling Metropolis",
           "Safari Gateway",
           "Christ the Redeemer"))

cities_sf = sf::st_as_sf(
  city_data_df,
  coords = c("longitude", "latitude"),
  crs = 4326
)
cities_sf

map_with_markers = leaflet(data = cities_sf) |>
  addProviderTiles(
    provider = providers$CartoDB.Positron
  ) |>
  addMarkers(
    popup = ~name
  ) |>
  setView(lng = 30, lat = 20, zoom = 2)
map_with_markers

# Adding polygons
france_sf = giscoR::gisco_get_countries(
  country = "FRA", resolution = "10"
)
france_sf
map_with_polygon = leaflet(france_sf) |>
  addProviderTiles(
    provider = providers$CartoDB.Positron
  ) |>
  addPolygons(fillColor = "darkslateblue",
              color = "#FFFFFF",
              weight = 1.5,
              fillOpacity = .5,
              popup = ~ NAME_ENGL) |>
  setView(lng = 2.3, lat = 46.8, zoom = 5)
map_with_polygon

# Customizing Popups with HTML ----
map_custom_popups = leaflet(cities_sf) |>
  addProviderTiles(
    provider = providers$CartoDB.Positron
  ) |>
  addMarkers(
    popup = ~ glue::glue(
      "<b>{name}</b><br/>",
      "<i>{info}</i>"
    )
  ) |>
  setView(lng = 30, lat = 20, zoom = 2)
map_custom_popups


# Giving Users Control: layer control ----
map_with_layers = leaflet() |>
  addProviderTiles(
    provider = providers$CartoDB.Positron,
    group = "Simple Map"
  ) |>
  addProviderTiles(
    provider = providers$OpenStreetMap.Mapnik,
    group = "OSM Map"
  ) |>
  addProviderTiles(
    provider = providers$Esri.WorldImagery,
    group = "Satellite"
  ) |>
  addPolygons(data = france_sf,
              fillColor = "darkslateblue",
              color = "white",
              weight = 1.5,
              fillOpacity = .5,
              popup = ~NAME_ENGL,
              group = "Countries") |>
  addMarkers(data = cities_sf,
             popup = ~ glue::glue(
               "<b>{name}</b><br/><i>{info}</i>"
             ),
             group = "Cities") |>
  addLayersControl(
    baseGroups = c("Simple Map", "OSM Map", "Satellite"),
    overlayGroups = c("Cities", "Countries"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  setView(lng = 2.3, lat = 46.8, zoom = 4)
map_with_layers

# Saving your interactive map ----
htmlwidgets::saveWidget(
  map_with_layers, file = "figures/interactive_map.html",
  selfcontained = TRUE
)
