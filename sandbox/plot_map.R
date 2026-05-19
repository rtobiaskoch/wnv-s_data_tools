plot_map <- function(
  df,
  long = "long",
  lat = "lat",
  id,
  color = "zone",
  palette = "Set2"
) {
  # Create a color palette based on the color column
  pal <- colorFactor(palette = palette, domain = df[[color]])

  # Create the Leaflet map
  map <- leaflet(df) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      lng = df[[long]],
      lat = df[[lat]],
      color = pal(df[[color]]),
      popup = paste0("Station: ", df[[id]], "<br>", color, ": ", df[[color]])
    ) %>%
    addLegend("bottomright", pal = pal, values = df[[color]], title = color)

  return(map)
}
