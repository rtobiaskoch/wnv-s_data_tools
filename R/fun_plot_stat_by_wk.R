library(ggplot)
library(dplyr)
library(RColorBrewer)


plot_stat_by_wk = function(df,y, species = "All", ymax = 4) {
  df %>%
    filter(spp == species) %>%
    filter(!zone %in% c("FC", "BC")) %>%
    mutate(zone = factor(zone, levels = c("NW", "NE", "SW", "SE", "LV","BE"))) %>%
    ggplot(aes(week,{{y}}, color = zone, group = year)) +
    geom_line() +
    coord_cartesian(ylim = c(0, ymax)) +
    geom_hline(yintercept = 0.75, color = "red", linetype = "dashed") +
    scale_color_brewer(palette = "Set2") +
    facet_wrap(~zone, ncol = 2) +
    theme_classic()
    
}

zone_stats = read.csv("1_input/wnv-s_zone_stats - data.csv")

plot_stat_by_wk(zone_stats, vi)

