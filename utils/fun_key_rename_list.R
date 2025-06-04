key_rename_list <- function(df, keychain = NULL, drop = T) {
  
  if(is.null(keychain)) {
    col_key <- list(
      trap_id   = c("trap_name"),
      trap_date = c("date_trap_set"),
      spp       = c("mosquito_species"),
      method    = c("trap_type"),
      total     = c("mosquito_count"),
      zone      = c("zone")
    )
  }
  
  # Map: new name → first matching old name in df
  rename_pairs <- purrr::map_chr(col_key, function(old_names) {
    found <- old_names[old_names %in% names(df)]
    if (length(found) > 0) found[1] else NA_character_
  })
  
  # Keep only matched names
  rename_pairs <- rename_pairs[!is.na(rename_pairs)]
  
  # Rename: new = old
  df_renamed <- df %>%
    dplyr::rename(!!!setNames(rename_pairs, names(rename_pairs)))
  
  if (drop) {
    df_renamed <- dplyr::select(df_renamed, dplyr::all_of(names(rename_pairs)))
  }
  
  return(df_renamed)
}