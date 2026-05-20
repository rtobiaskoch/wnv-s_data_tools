plot_n_trap = function(df, rm_zone = NULL) {
  if (!"zone2" %in% names(df)) {
    stop("`df` must have a `zone2` column — run wnv_s_clean() first.")
  }

  df %>%
    filter(!zone %in% rm_zone) %>%
    # Collapse duplicate rows per trap-week to one representative trap_status.
    # The join can produce multiple rows per (trap_id, year, week) when both the
    # culex sheet and the database have observations — each with a different
    # trap_date and therefore a different trap_status after wnv_s_clean.
    # Priority: malfunction > culex > no culex > no mosquitoes.
    group_by(trap_id, year, week, zone2) %>%
    mutate(
      trap_status = case_when(
        any(trap_status == "malfunction", na.rm = TRUE) ~ "malfunction",
        any(trap_status == "culex", na.rm = TRUE) ~ "culex",
        any(trap_status == "no culex", na.rm = TRUE) ~ "no culex",
        TRUE ~ "no mosquitoes"
      )
    ) %>%
    ungroup() %>%
    group_by(year, week, zone2, trap_status) %>%
    summarise(n = n_distinct(trap_id), .groups = "drop") %>%
    ggplot(aes(week, n, color = trap_status, fill = trap_status)) +
    geom_col(alpha = 0.7) +
    scale_fill_manual(values = trap_status_colors, na.value = "grey80") +
    scale_color_manual(values = trap_status_colors, na.value = "grey80") +
    scale_x_continuous(
      breaks = seq(23, 37, by = 5),
      limits = c(23, 37)
    ) +
    facet_grid(zone2 ~ year, scales = "free_y") +
    # scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, NA)) +
    theme_classic()
}
