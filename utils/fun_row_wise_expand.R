library(dplyr)
library(tidyr)
library(rlang)

rowwise_expand <- function(df, start, end, new_name) {
  
  # Convert inputs to symbols if they are not already
  start_sym <- ensym(start)
  end_sym <- ensym(end)
  new_col <- as_string(ensym(new_name))
  
  df_expanded <- df |>
    rowwise() |>
    mutate(!!new_col := list(seq(!!start_sym, !!end_sym))) |>
    unnest(!!sym(new_col)) |>
    ungroup()
  
  return(df_expanded)
}
