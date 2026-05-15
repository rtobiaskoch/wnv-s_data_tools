plot_culex_datasheet_comp = function(df, y) {
  df %>% 
  filter(year > y) %>%
  group_by(year, week, source) %>%
  summarize(total = sum(total), .groups = "drop") %>%
  ggplot( aes(x = week, y = total, color = source)) +
  geom_point(alpha = 0.5) +
  geom_line() +
  theme_classic() +
  facet_wrap(~year)
}
