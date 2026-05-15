plot_map <- function(df, 
                     long = "long", 
                     lat = "lat", 
                     id,
                     color,
                     palette = "Set2") {
  
  # Create a color palette based on the color column
  pal <- colorFactor(palette = palette, domain = df[[color]])
  
  # Create the Leaflet map
  map <- leaflet(df) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      lng = ~get(long),
      lat = ~get(lat),
      color = ~pal(get(color)),
      popup = ~paste0("Station: ", get(id), 
                      "<br>", color, ": ", get(color))
    ) %>%
    addLegend("bottomright", 
              pal = pal, 
              values = ~get(color), 
              title = color)
  
  return(map)
}