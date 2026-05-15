get_trap_n<- function(data, 
                      year_filter, 
                      grp, #group to calculate max distinct trap n by
                      zone_filter = NULL,
                      zone_rename = NULL) {
  
  
  grp_sym = rlang::syms(grp)
  
  data %>%
    filter(year %in% year_filter) %>%
    filter(method == "L") %>%
    {
      if (!is.null(zone_filter)) filter(., zone %in% zone_filter) else .
    } %>%
    {
      if (!is.null(zone_rename)) mutate(., zone = zone_rename) else .
    } %>%
    distinct(year, trap_date, week, zone, trap_id) %>%
    group_by(!!!grp_sym) %>%
    summarise(trap_L = n(), .groups = "drop") %>%
    group_by(year, zone) %>%
    summarise(trap_L = max(trap_L, na.rm = TRUE), .groups = "drop")
}