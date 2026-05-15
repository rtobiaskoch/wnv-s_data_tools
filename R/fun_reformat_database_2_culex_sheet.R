reformat_database_2_culex_sheet = function(df){
  df %>%
    dplyr::group_by(trap_id, trap_date, year, week, zone, spp, method) %>%
    dplyr::summarise(total = sum(total),
              .groups = "drop"
              ) %>%
    mutate(trap_date = as.Date(trap_date))
    
}