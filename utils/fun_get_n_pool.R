library(dplyr)

get_n_pool = function(df, wks = 1:52, n) {
  df |>
    filter(week %in% wks) |>
    filter(total == n)
}