library(tidyverse)

plot_stats_time = function(df, kp_stat = c("abund", "pir", "vi"), yr = 1999,
                           abund_factor = 10, pir_factor = 1000) {
  df = df %>%
    filter(.data$year > yr) %>%
    group_by(.data$year, week) %>%
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(abund = abund/abund_factor,
           pir = pir*pir_factor) %>%
    pivot_longer(cols = all_of(kp_stat),
                 names_to = "stat",
                 values_to = "relative value") %>%
    filter(stat %in% kp_stat)
  
  p = ggplot(df, aes(x = week, y = `relative value`, color = stat, fill = stat)) +
    geom_area(alpha = 0.5, position = "dodge") +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(~year, nrow = 1) +
    theme_classic() +
    theme(legend.position = "right",
          legend.title = element_blank())
  
  return(p)
}

#data zone stats with updated abundance
df = read.csv("../wnv-ss-wkly_report/3_output/zone_stats.csv")
cases =  read.csv("1_input/larimer_county_cases.csv")

case_abund = left_join(df, cases, by = "year") %>%
  mutate(case = case/50)


p = plot_stats_time(case_abund %>% filter(year <2024)
                    , kp_stat = c("case", "vi"), yr = 2014) +
  scale_y_continuous(
                     name = "Vector Index",
                    sec.axis = sec_axis(~ . * 50, name = "Human casess")
)
  
p 


ggsave("3_output/p_stats_over_time.png")


#data zone stats goes that to 2015
#df = read.csv("1_input/data_zone_wk_stats.csv")
#plot_stats_time(df, kp_stat = c("abund", "pir", "cases"))



plot_stats_time_all_wk = function(df, kp_stat = c("abund", "pir", "vi"), yr = 1999,
                                  abund_factor = 10, pir_factor = 1000) {
  df = df %>%
    filter({{year}} > yr) %>%
    filter(spp == 'All') %>%
    group_by(year, week) %>%
    summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(abund = abund/abund_factor,
           pir = pir*pir_factor) %>%
    pivot_longer(cols = -c(year, week),
                 names_to = "stat",
                 values_to = "relative value") %>%
    filter(stat %in% kp_stat)
  
  df1 = expand.grid(year = min(df$year):max(df$year),
                    week = 1:52,
                    stat = kp_stat) %>%
    left_join(df, by = c("year", "week", "stat")) %>%
    arrange(year, week)
  
  df1[is.na(df1)] = 0
  #browser()
  
  p = ggplot(df1, aes(x = week, y = `relative value`, color = stat, fill = stat)) +
    geom_area(alpha = 0.5, position = "dodge") +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(~year, nrow = 1) +
    scale_x_continuous(breaks = seq(10, 50,10)) +
    theme_classic() +
    theme(legend.position = "right",
          legend.title = element_blank())
  
  return(p)
}
p = plot_stats_time_all_wk(case_abund, kp_stat = c("abund","pir"), yr = 2019)
p







