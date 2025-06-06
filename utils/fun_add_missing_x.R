add_missing_x <- function(data,
                          group_vars = c("trap_id", "trap_date"),
                          status_col = "trap_status",
                          status_val = "no culex",
                          add_col = "spp",
                          add_vals = c("Pipiens", "Tarsalis"),
                          keep_only_target_status = TRUE) {
  
  group_syms <- rlang::syms(group_vars)
  status_sym <- rlang::sym(status_col)
  add_sym <- rlang::sym(add_col)
  
  relevant_groups <- data |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::filter(any(!!status_sym == status_val)) |>
    dplyr::ungroup()
  
  if (keep_only_target_status) {
    relevant_groups <- relevant_groups |>
      dplyr::filter(!!status_sym == status_val)
  }
  
  injected_rows <- relevant_groups |>
    dplyr::group_by(!!!group_syms) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-!!add_sym, -total) |>  # <-- Drop conflicting cols early
    dplyr::rowwise() |>
    dplyr::mutate(new_data = list(
      tibble(!!add_col := add_vals, total = 0)
    )) |>
    tidyr::unnest(new_data) |>
    dplyr::relocate(!!add_sym, total, .after = last_col())
  
  return(injected_rows)
}
