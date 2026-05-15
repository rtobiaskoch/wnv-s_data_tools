library(dplyr)

#' Remove groups where all values in a target column are NA
#'
#' @param df A data frame or tibble
#' @param group_vars A character vector of column names to group by
#' @param target_col A character string naming the column to check for NA
#'
#' @return A filtered data frame where any group with all NAs in `target_col` is removed
#'
#' @examples
#' df <- tibble(
#'   id = c(1, 1, 2, 2, 3, 3),
#'   time = c("A", "B", "A", "B", "A", "B"),
#'   value = c(NA, NA, 10, NA, 5, 6)
#' )
#'
#' # Remove groups where all `value` entries are NA, grouped by `id`
#' remove_all_na_groups(df, group_vars = c("id"), target_col = "value")
#'
rm_grp_na <- function(df, group_vars, target_col) {
  
  # Check inputs
  if (!all(group_vars %in% names(df))) {
    stop("All group_vars must be column names in the dataframe.")
  }
  if (!(target_col %in% names(df))) {
    stop("target_col must be a column name in the dataframe.")
  }
  
  df_new = df %>%
    group_by(across(all_of(group_vars))) %>%
    filter(!all(is.na(.data[[target_col]]))) %>%
    ungroup()
  
  
  cat("\nRemoved ", nrow(df) - nrow(df_new), " rows with missing ", target_col, 
      " grouped by ", group_vars, "\n")
  
  return(df_new)
  
}
