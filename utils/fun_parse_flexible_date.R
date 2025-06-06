parse_flexible_date <- function(date_str) {
  # Return NA immediately if input is NA
  if (is.na(date_str)) return(NA_Date_)
  
  # If already a Date, ensure it's formatted as YYYY-MM-DD
  if (inherits(date_str, "Date")) return(as.Date(format(date_str, "%Y-%m-%d")))
  
  # Validate input
  if (!is.character(date_str) || length(date_str) != 1) {
    stop("`date_str` must be a single character string.")
  }
  
  # If it already looks like a proper ISO date, return it if valid
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) {
    iso_date <- suppressWarnings(as.Date(date_str, format = "%Y-%m-%d"))
    if (!is.na(iso_date)) return(iso_date)
  }
  
  # Common formats with 2-digit years
  formats_2digit <- c("%m/%d/%y", "%d/%m/%y", "%y-%m-%d", "%y/%m/%d")
  
  # Expand 2-digit years manually
  for (fmt in formats_2digit) {
    dt <- suppressWarnings(as.Date(date_str, format = fmt))
    if (!is.na(dt)) return(dt)
  }
  
  # Other full 4-digit year formats
  formats <- c(
    "%m/%d/%Y", "%m-%d-%Y",
    "%Y/%m/%d", "%d/%m/%Y", "%d-%m-%Y",
    "%Y.%m.%d", "%m.%d.%Y",
    "%b %d %Y", "%d %b %Y",
    "%B %d %Y", "%d %B %Y"
  )
  
  for (fmt in formats) {
    dt <- suppressWarnings(as.Date(date_str, format = fmt))
    if (!is.na(dt)) return(dt)
  }
  
  # Could not parse
  return(NA_Date_)
}
