join_merge_z = function(df_z1, df_z2, z) {
 
  join_col = setdiff(colnames(df_z1), z)
  
  full = full_join(df_z1, df_z2, by = join_col) %>%
    mutate(total = if_else(is.na(total.x), total.y, total.x)) %>%
    mutate(check = if_else(!is.na(total.x) & total.x!= total.y, T, F)) %>%
    arrange(desc(check))
  
  final = full %>%
    select(-c(total.x, total.y, check))
  
  list(full = full, 
       final = final)

}