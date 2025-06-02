plot_n_trap = function(df, rm_zone) {
 df %>% 
  dplyr::mutate(zone = dplyr::if_else(zone %in% c("NW", "SW", "NE", "SE"), 
                                      "FC", 
                                      zone)) %>%
  filter(!zone %in% rm_zone) %>%
  group_by(year, week, zone) %>%
  summarise(n = n_distinct(trap_id),
            .groups = "drop") %>%
  ggplot(aes(week, n, color = zone)) +
  geom_line() +
  scale_x_continuous(
      breaks = seq(23, 37, by = 2) ) +
  facet_wrap(~year) +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  theme_classic()
}