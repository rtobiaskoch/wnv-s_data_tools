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
  
  required_cols <- c("trap_id", "zone", "trap_date", "spp", "method", "total")
  present_cols <- intersect(required_cols, names(df))
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    message("Warning: following columns are missing from input data be sure to run culex_clean on raw data: ", 
            paste(missing_cols, collapse = ", "))
  }

  #if na_col is a symbol keep it that way else convert to symbol
  if(rlang::is_symbol(na_col)){
    col_sym <- na_col
  } else {
    col_sym <- rlang::ensym(na_col)
  }
  col_label <- rlang::as_label(col_sym)
  
  #REMOVE MISSING DATA
  data <- df %>%
    dplyr::filter(!is.na(!!col_sym)) %>%
    dplyr::filter(nchar(!!col_sym) != 0)
  
  cat("\nFiltered out", nrow(df) - nrow(data), "observations with missing", col_label, ",", nrow(data), "remaining.\n")
  
  #REMOVE DUPLICATES
  data2 <- dplyr::distinct_all(data)
  
  cat("\nFiltered out", nrow(data) - nrow(data2), "duplicates,", nrow(data2), "remaining.\n")
 
  #REMOVE NON CULEX SPECIES
  data3 = data2 %>%
    filter(str_detect(str_to_lower(spp), 
                      pattern = str_to_lower("Tarsalis|Pipiens|No Mosquitoes|Trap Malfunction|Trap Stolen")))
  
  cat("\nFiltered out", nrow(data2) - nrow(data3), "non culex species,", nrow(data2), "remaining.\n")
  
  #REMOVE NON-ACTIVE TRAPS 
  if (!is.null(trap_keep_df)) {
    data4 <- dplyr::semi_join(data3, trap_keep_df, by = rlang::set_names(rlang::quo_name(col_sym)))
    
    if(length(unique(data4)) != length(unique(data3)) ){
      stop(cat("Warning some traps from foco_trap were not found in the culex sheet. check the semi_join in filter_culex_sheet."))
    }
    
    cat("\nRemoved", nrow(data3) - nrow(data4), "observations from",
        length(unique(dplyr::pull(data3, !!col_sym))) - length(unique(dplyr::pull(data4, !!col_sym))),
        "non-testing traps,", nrow(data4), "observations remaining.\n")
  } else {
    data4 <- data3
  }
  
  return(data4)
}
