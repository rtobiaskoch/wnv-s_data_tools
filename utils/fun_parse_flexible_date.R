#' Parse Dates with Multiple Possible Formats
#'
#' Attempts to parse a date string using a range of common date formats
#' (U.S., ISO, European, and formats with full/abbreviated month names).
#'
#' @param date_str A character string representing a date.
#'   Can be in a variety of formats (e.g., "2024-06-10", "06/10/2024", "10 Jun 2024").
#'
#' @return A `Date` object if parsing is successful, or `NA` if no format matches.
#'
#' @examples
#' parse_flexible_date("2024-06-10")
#' parse_flexible_date("10/06/2024")
#' parse_flexible_date("June 10 2024")
#'
#' @export
parse_flexible_date <- function(date_str) {
  # Return NA immediately if input is NA
  if (is.na(date_str)) return(NA_Date_)
  
  #if its already a date do stuff else nothing
  if(!is.Date(date_str)) {
    
    # Validate input
    if (!is.character(date_str) || length(date_str) != 1) {
      stop("`date_str` must be a single character string.")
    }
    
    # Define common date formats
    formats <- c(
      "%m/%d/%Y", "%m-%d-%Y",     # U.S.
      "%Y-%m-%d", "%Y/%m/%d",     # ISO
      "%d/%m/%Y", "%d-%m-%Y",     # European
      "%Y.%m.%d", "%m.%d.%Y",     # Dot formats
      "%b %d %Y", "%d %b %Y",     # Abbreviated month
      "%B %d %Y", "%d %B %Y"      # Full month
    )
    
    # Try parsing using all formats
    parsed_dates <- purrr::map(formats, ~ suppressWarnings(as.Date(date_str, format = .x)))
    valid_dates <- purrr::keep(parsed_dates, ~ !is.na(.x))
    
    # Return first successful parse or NA
    if (length(valid_dates) == 0) NA_Date_ else valid_dates[[1]]
    
  } else {
    return(date_str)
  } # end if date

}