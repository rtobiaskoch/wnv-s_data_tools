#' Filter Culex Data Sheet
#'
#' Filters a data frame by removing rows with missing or blank values in a specified column,
#' removes duplicates, and optionally retains only rows present in a `trap_keep_df` based on that column.
#'
#' @param df A data frame containing the Culex data.
#' @param na_col The name of the column (unquoted) to check for missing or blank values.
#' @param trap_keep_df Optional. A data frame containing trap IDs to retain (must have a column matching `na_col`).
#'
#' @return A filtered data frame with informative messages printed to console.
#' @examples
#' \dontrun{
#' filter_culex_sheet(mydata, trap_id, trap_reference_df)
#' }
#' @importFrom dplyr filter distinct_all semi_join pull
#' @importFrom rlang ensym as_label quo_name
#' @export

filter_culex_sheet <- function(df, na_col, trap_keep_df = NULL) {

  #if na_col is a symbol keep it that way else convert to symbol
  if(rlang::is_symbol(na_col)){
    col_sym <- na_col
  } else {
    col_sym <- rlang::ensym(na_col)
  }
  col_label <- rlang::as_label(col_sym)
  
  data <- df %>%
    dplyr::filter(!is.na(!!col_sym)) %>%
    dplyr::filter(nchar(!!col_sym) != 0)
  
  cat("\nFiltered out", nrow(df) - nrow(data), "observations with missing", col_label, ",", nrow(data), "remaining.\n")
  
  data2 <- dplyr::distinct_all(data)
  
  cat("\nFiltered out", nrow(data) - nrow(data2), "duplicates,", nrow(data2), "remaining.\n")
  
  if (!is.null(trap_keep_df)) {
    data3 <- dplyr::semi_join(data2, trap_keep_df, by = rlang::set_names(rlang::quo_name(col_sym)))
    
    cat("\nRemoved", nrow(data2) - nrow(data3), "observations from",
        length(unique(dplyr::pull(data2, !!col_sym))) - length(unique(dplyr::pull(data3, !!col_sym))),
        "non-testing traps,", nrow(data3), "observations remaining.\n")
  } else {
    data3 <- data2
  }
  
  return(data3)
}
