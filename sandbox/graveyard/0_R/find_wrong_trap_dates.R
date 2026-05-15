
#df is the wnv_database
#find traps with two dates for the same week
find_wrong_trap_dates = function(df) {
  df %>%
    distinct(year,trap_date, week, zone, trap_id, method) %>% #get unique number of traps by removing traps listed 2x+ with multiple pools and spp
    group_by(year, zone, week, method, trap_id) %>% #get number of traps per week per zone
    summarise(n = n()) %>%
    filter(n >1)
  
}
