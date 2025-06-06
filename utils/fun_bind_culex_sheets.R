bind_culex_sheets = function(culex, database) {
  culex = culex %>% mutate(source = "culex")
  database = database %>% mutate(source = "datasheet")
  bind_rows(culex, database)
}