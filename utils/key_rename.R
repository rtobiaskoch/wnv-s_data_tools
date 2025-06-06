library(dplyr)


key_rename <- function(df, rename_df, drop_extra = FALSE) {
  # Ensure proper column names
  if (!all(c("old", "new") %in% names(rename_df))) {
    stop("rename_df must contain columns: 'old' and 'new'")
  }
  
  # Filter to only applicable old names in df
  valid_map <- rename_df %>%
    filter(old %in% names(df))
  
  # Build rename pairs: new = old
  rename_pairs <- setNames(valid_map$old, valid_map$new)
  
  # Apply renaming
  df_renamed <- df %>%
    dplyr::rename(!!!rename_pairs)
  
  # Optionally drop unmatched columns
  if (drop_extra) {
    df_renamed <- dplyr::select(df_renamed, dplyr::all_of(valid_map$new))
  }
  
  return(df_renamed)
}
