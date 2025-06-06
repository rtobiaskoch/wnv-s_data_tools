plot_n_trap = function(df, rm_zone = NULL) {
 df %>% 
  filter(!zone %in% rm_zone) %>%
  group_by(year, week, zone2, trap_status) %>%
  summarise(n = n_distinct(trap_id),
            .groups = "drop") %>%
  ggplot(aes(week, n, color = trap_status, fill = trap_status)) +
  geom_col(alpha = 0.7) +
  scale_x_continuous(
      breaks = seq(23, 37, by = 2) ) +
  facet_grid(zone2~year) +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  theme_classic()
}