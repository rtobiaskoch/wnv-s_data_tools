#df is the wnv_database
#replaces multiple trap dates for a given week with the max
fix_trap_dates = function(df) {
  
  df %>%
    group_by(year, week, zone, trap_id) %>%
    mutate(
      trap_date = if (n_distinct(trap_date) > 1) {
        max(trap_date) # Replace with the earliest date
      } else {
        trap_date
      }
    ) %>%
    ungroup()
}

get_changed_dates = function(df,df_update) {
  df %>%
    mutate(updated_trap_date = df_update$trap_date) %>%
    group_by(year, week, zone, trap_id) %>%
    filter(any(trap_date != updated_trap_date)) %>%
    select(year, week, zone, trap_id, csu_id, trap_date, updated_trap_date) %>%
    ungroup
}