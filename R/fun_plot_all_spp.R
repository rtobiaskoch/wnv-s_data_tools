plot_all_spp = function(df, z) {
  df = df %>%
    dplyr::filter(!is.na(.data[[z]])) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(z)), year, week, spp0) %>%
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

  ggplot(df, aes(x = week, y = total, fill = spp0)) +
    geom_col() +
    facet_grid(reformulate("year", z)) +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank())
}
