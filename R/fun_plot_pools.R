plot_pools = function(df) {
 df %>%
  mutate(n_pools= n_pools - n_pos_pools) %>%
  pivot_longer(cols = c(n_pools, n_pos_pools)) %>%
  filter(zone != "FC") %>%
  mutate(name = factor(name, 
                       levels = c( "n_pos_pools","n_pools"),
                       labels = c("Positive Pools","Total Pools"))) %>%
  ggplot(aes(x = zone, y = value, fill = name, color = name)) +
  geom_col() +  # Add dodging for side-by-side bars
  theme_classic() +
  labs(fill = "Pool Status", color = "Pool Status")  # Better legend title
}
