library(dplyr)


key_rename <- function(df, rename_df, drop_extra = FALSE) {
  # Ensure proper column names
  if (!all(c("old_name", "new_name") %in% names(rename_df))) {
    stop("rename_df must contain columns: 'old_name' and 'new_name'")
  }
  
  # Filter to only applicable old names in df
  valid_map <- rename_df %>%
    filter(old_name %in% names(df))
  
  # Build rename pairs: new = old
  rename_pairs <- setNames(valid_map$old_name, valid_map$new_name)
  
  # Apply renaming
  df_renamed <- df %>%
    dplyr::rename(!!!rename_pairs)
  
  # Optionally drop unmatched columns
  if (drop_extra) {
    df_renamed <- dplyr::select(df_renamed, dplyr::all_of(valid_map$new_name))
  }
  
  return(df_renamed)
}
