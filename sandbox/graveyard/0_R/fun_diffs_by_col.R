diffs_by_col <- function(og_df, new_df, columns, id) {

  
  columns = setdiff(columns, id) #ensure id is not in your list or will throw error in anti_join
  
  purrr::map(columns, function(col_name) {
    
    new_df = new_df |> 
      mutate_all(as.character) |>  #convert all col to character to avoid class error in antijoin
      select(id, col_name) #create short list for left_join
    
    og_df |>
      select(id, col_name) |> #keep only comparison easier viewing
      mutate_all(as.character) |> #convert all col to character to avoid class error in antijoin
      dplyr::anti_join(new_df, by = c(id, col_name)) |> #keep only variables in og_df that are not in new_df
      dplyr::left_join(new_df, by = id) # get values from og_df and new_df
                                         }) |>
    rlang::set_names(columns)
}