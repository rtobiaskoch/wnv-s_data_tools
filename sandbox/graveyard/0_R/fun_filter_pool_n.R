filter_pool_n = function(df, yr, wk = 1:52, n = 1) {
  df = df %>%
    filter(year %in% yr) %>%
    filter(week %in% wk) %>%
    filter(total %in% n)
  
  return(df)
}