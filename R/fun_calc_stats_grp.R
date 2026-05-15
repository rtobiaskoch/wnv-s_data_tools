calc_stats_grp <- function(df, 
                           grp_vars,         # grouping variables as strings (excluding or including col)
                           pattern,          # values to replace
                           pattern_replace,  # replacement value
                           col,              # name of column to collapse + group by
                           sum_col = c("n_pos_pool", "mosq_L",
                                       "abund", "abund_sd", "abund_lci", "abund_uci",
                                       "pir", "pir_lci", "pir_uci",
                                       "vi", "vi_lci", "vi_uci"),
                           distinct_col = c("trap_L")) {
  
  col_sym <- rlang::ensym(col)
  col_chr <- rlang::as_string(col_sym)
  full_grp_vars <- rlang::syms(unique(c(grp_vars, col_chr)))
  
  df %>%
    mutate(
      {{ col_sym }} := if_else(
        {{ col_sym }} %in% pattern,
        pattern_replace,
        as.character({{ col_sym }})
      )
    ) %>%
    group_by(!!!full_grp_vars) %>%
    summarise(
      across(all_of(sum_col), ~ sum(.x, na.rm = TRUE)),
      across(all_of(distinct_col), ~ max(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(across(everything(), ~ replace(.x, is.na(.x), 0)))
}
