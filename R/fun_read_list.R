#' Read and Combine Files Matching a Pattern
#'
#' This function searches recursively for files matching a given pattern,
#' imports them using `rio::import()`, converts all columns to character,
#' and combines them into a single data frame.
#'
#' @param pattern A character string used to match file names (e.g., ".*\\.csv$").
#'   The pattern is passed to `list.files()` for recursive matching.
#'
#' @return A single data frame combining all matched and processed files.
#'
#' @examples
#' \dontrun{
#' combined_df <- fun_read_list(".*\\.csv$")
#' }
#'
#' @export
read_list <- function(path, pattern) {
  # Input validation
  if (missing(pattern) || !is.character(pattern) || length(pattern) != 1) {
    stop("`pattern` must be a single character string.")
  }
  
  # Find files
  file_list <- list.files(
    path = path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Find files
  file_name <- basename(file_list)
  
  if (length(file_list) == 0) {
    warning("No files matched the pattern: ", pattern)
    return(tibble::tibble())  # Return empty tibble
  } else {
    cat("\nReading in ", length(file_list), "files with pattern ", pattern, "\n")
  }
  
  # Read and process files
  df_list <- file_list %>%
    purrr::map(~ rio::import(.x)) %>%
    purrr::map(~ dplyr::mutate_all(., as.character))
  
  cols <- purrr::map_int(df_list, ncol)
  rows <- purrr::map_int(df_list, nrow)
  
  # Combine each triplet into one tab-separated line
  summary_lines <- pmap_chr(list(file_name, rows, cols), ~ paste(..., sep = "\t"))
  
  # Print all lines
  cat("\nImported files have the following number of rows and columns:\n\n",
      paste(summary_lines, collapse = "\n"))
  
  combined_df <- df_list %>%
    dplyr::bind_rows()
  
  cat("\n\nFinal combined data has the following dimensions:", dim(combined_df), "\n")
  
  if(ncol(combined_df) != round(mean(cols),2)) {
    cat("\nWarning number of columns in combined_df ", ncol(combined_df), 
        "and average in df_list", mean(cols), "don't match.\n")
  }
  
  return(combined_df)
}
