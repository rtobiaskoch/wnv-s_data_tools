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
  
  if (length(file_list) == 0) {
    warning("No files matched the pattern: ", pattern)
    return(tibble::tibble())  # Return empty tibble
  } else {
    cat("\nReading in ", length(file_list), "files with pattern ", pattern, "\n")
  }
  
  # Read and process files
  combined_df <- file_list %>%
    purrr::map(~ rio::import(.x)) %>%
    purrr::map(~ dplyr::mutate_all(., as.character)) %>%
    dplyr::bind_rows()
  
  cat("\nSuccessfully read in", nrow(combined_df), "observations.\n")
  
  return(combined_df)
}
