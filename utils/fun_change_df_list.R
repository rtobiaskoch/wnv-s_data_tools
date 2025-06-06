anti_join_by_columns <- function(df1, df2, columns, id) {
  
  columns = setdiff(columns, id) #ensure id is not in your list or will throw error in anti_join
  
  purrr::map(columns, function(col_name) {
    
    df2 = df2 |> 
      mutate_all(as.character) |>  #convert all col to character to avoid class error in antijoin
      select(id, col_name) #create short list for left_join
    
    df1 |>
      select(id, col_name) |> #keep only comparison easier viewing
      mutate_all(as.character) |> #convert all col to character to avoid class error in antijoin
      dplyr::anti_join(df2, by = c(id, col_name)) |> #keep only variables in df1 that are not in df2
      dplyr::left_join(df2, by = id) # get values from df1 and df2
                                         }) |>
    rlang::set_names(columns)
}