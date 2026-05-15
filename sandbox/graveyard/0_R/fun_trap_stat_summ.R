trap_stat_sum = function(df) {
  
  df = df %>%
  dplyr::filter(method == "L") %>%  # only run on Light Traps
  distinct(trap_id, zone, trap_status) %>%
  group_by(zone, trap_status) %>%
  count()
  
  p = df %>%
    ggplot(aes(x = zone, y = n, fill = trap_status, color = trap_status)) +
    geom_col(alpha = 0.7) +
    scale_fill_manual(values = trap_status_colors, na.value = "grey80") +
    scale_color_manual(values = trap_status_colors, na.value = "grey80") +
    theme_classic()
  
  df_long = df %>%
  pivot_wider(names_from = trap_status, 
              values_from = n, 
              values_fill = 0) %>%
  ungroup %>%
  mutate(trap_L = rowSums(select(., where(is.numeric))))
  
  
  output = list("p" = p,
                df = df_long)
  
  return(output)
}