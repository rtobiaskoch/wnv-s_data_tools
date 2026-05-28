plot_all_spp = function(df, z, spp_col = "spp0") {
  df = df %>%
    dplyr::filter(!is.na(.data[[z]])) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(z, spp_col))), year, week) %>%
    dplyr::summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

  ggplot(df, aes(x = week, y = total, fill = .data[[spp_col]])) +
    geom_col() +
    facet_grid(reformulate("year", z)) +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank())
}
