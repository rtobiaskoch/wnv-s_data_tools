join_culex_sheets = function(culex, database) {
  culex = culex %>% 
    mutate(source = "culex",
           trap_date = as.Date(trap_date)) %>%
    rename(c_total = total)
  
    database = database %>% 
    mutate(source = "datasheet",
           trap_date = as.Date(trap_date)) %>%
    rename(d_total = total)
  
  full_join(culex, database)
}