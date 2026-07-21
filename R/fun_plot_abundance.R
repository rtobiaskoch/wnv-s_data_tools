plot_abund = function(df) {
  ggplot(df, aes(x = week, y = abund, fill = spp)) +
    geom_area() +
    facet_grid(zone ~ year, scales = "free") +
    theme_classic()
}
