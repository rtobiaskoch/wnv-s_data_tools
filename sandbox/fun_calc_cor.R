library(tidyverse)
library(ggplot2)
library(patchwork)

d = read.csv("3_output/zone_stats_v2.csv") %>%
  filter(!is.na(abund) & !is.na(pir)) %>%
  filter(year > 2016) %>%
  group_by(year) %>%
  summarise(pir = max(pir), abund = max(abund))

attach(d)

cr = round(cor(abund, pir), 2)

p = ggplot(d, aes(abund, pir)) +
  geom_point(aes(color = as.factor(year))) +
  geom_smooth(method = "lm") +
  theme_classic() +
  ggtitle(paste0("r = ", cr))

ggsave(filename = "3_output/zone_stat_corr.png", plot = p)


#within season by week correlation
#ccf(abund, pir, lag.max = 10, plot = TRUE, na.action = na.pass)

# Function to generate lagged scatterplots
#plot_lagged_scatter <- function(data,x,y, max_lag = 4) {
plots <- list()

for (lag in 0:max_lag) {
  data_lag <- data %>%
    mutate(x_lagged = dplyr::lag({{ x }}, lag))

  p <- ggplot(data_lag, aes(x = x_lagged, y = {{ y }})) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(
      title = paste("Lag", lag),
      x = ifelse(lag == 0, "x[t]", paste0("x[t-", lag, "]"))
    ) +
    theme_minimal()

  plots[[lag + 1]] <- p
}

wrap_plots(plots, ncol = 2) # Arrange in 2 columns

# Generate plots for lags 0 through 4
#p = plot_lagged_scatter(d, pir, abund, max_lag = 10)
