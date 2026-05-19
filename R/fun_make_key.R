#' Create a composite key column from specified columns
#'
#' This function takes a data frame and a character vector of column names
#' that should be combined to form a unique key. It verifies that all provided
#' columns exist in the data frame and then creates a new column `key` by
#' concatenating the values with a pipe (`|`) separator. The original data
#' frame is returned with the new key column added.
#'
#' @param df A data frame.
#' @param key_cols A character vector of column names to use for the key. Must contain at least two column names.
#' @param name A character name for the key column
#' @param sep seperator for the combined key
#'   At least two column names should be supplied.
#' @return The input data frame with an additional `key` column.
#' @examples
#' make_key(df, c("trap_id", "spp", "year", "week"))
# Load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
     stop("Package 'dplyr' is required for make_key(). Please install it.")
}
if (!requireNamespace("rlang", quietly = TRUE)) {
     stop("Package 'rlang' is required for make_key(). Please install it.")
}

make_key = function(
     df,
     key_cols,
     name = "key",
     sep = "|"
) {
     # Ensure key_cols is a character vector with at least two elements
     if (missing(key_cols) || length(key_cols) < 2) {
          stop("Please provide at least two column names for the key.")
     }
     # Check that all specified columns exist in the data frame
     missing_cols <- setdiff(key_cols, colnames(df))
     if (length(missing_cols) > 0) {
          stop(paste(
               "The following key columns are missing from the data frame:",
               paste(missing_cols, collapse = ", ")
          ))
     }
     # Clean each key column value for key construction only — originals are unchanged
     cleaned_parts <- purrr::map(
          key_cols,
          ~ gsub("[^[:alnum:]]", "", as.character(df[[.x]]))
     )

     dplyr::mutate(
          df,
          # sep is inserted as-is, preserving non-alphanumeric separators like "|"
          !!rlang::sym(name) := do.call(paste, c(cleaned_parts, sep = sep))
     ) %>%
          dplyr::select(!!rlang::sym(name), dplyr::everything())
}
