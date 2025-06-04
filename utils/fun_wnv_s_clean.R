#' Clean a Culex Surveillance Data Sheet
#'
#' Processes and standardizes raw Culex mosquito surveillance data.
#' Trims whitespace from character columns, parses collection dates using
#' parse_flexible_date(), assigns species and method categories, and
#' returns a cleaned data frame with selected columns.
#'
#' @param df A data frame containing raw Culex surveillance data. Must include the columns:
#' trap_name, date_trap_set, mosquito_species, trap_type, mosquito_count, and zone.
#'
#' @return A cleaned and standardized data frame with columns:
#' trap_id, trap_date, year, week, zone, spp, method, and total.
#'
#' @details This function depends on parse_flexible_date() and col_rename() which must be defined elsewhere
#' in the package or user's environment.
#'
#' @examples
#' \dontrun{
#' clean_df <- culex_sheet_clean(raw_df)
#' }
#'
#' @importFrom dplyr mutate across transmute case_when
#' @importFrom stringr str_detect
#' @importFrom purrr map_chr
#' @importFrom lubridate year week
#' @export




wnv_s_clean <- function(df) {
  
  # Check required cleaned columns
  required_cols <- c("trap_id", "zone", "trap_date", "spp", "method", "total")
  present_cols <- intersect(required_cols, names(df))
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    message("Warning: missing columns after rename: ", paste(missing_cols, collapse = ", "))
  }
  
  # Trim whitespace from all character columns
  df <- df %>%
    mutate(across(where(is.character), trimws))
  


  
  if("zone" %in% names(df)) {
    
    valid_zones <- c("NE", "NW", "SE", "SW", "LV", "BE", "BC")
    # Create regex pattern like "NE|NW|SE|SW|LV|BE|BC"
    zone_pattern <- str_c(valid_zones, collapse = "|")
    
    df <- df %>%
      mutate(zone = str_extract(zone, zone_pattern))
  }
  
  if ("trap_date" %in% names(df)) {
    if (!exists("parse_flexible_date", mode = "function")) {
      stop("Missing function `parse_flexible_date()`. This is a personal utility function you need to define or import (e.g., from utils/mod-dates.r).")
    }
    
    df <- df %>%
      mutate(
        trap_date = purrr::map_chr(trap_date, ~ as.character(parse_flexible_date(.x))),
        trap_date = as.Date(trap_date)
      )
  }
  
  # Derive year and week from trap_date
  if ("trap_date" %in% names(df)) {
    df <- df %>%
      mutate(
        year = lubridate::year(trap_date),
        week = lubridate::week(trap_date)
      )
  }
  
  # Standardize trap_id and assign method
  #GET METHOD
  if ("trap_id" %in% names(df)) {
    df <- df %>%
      mutate(
        trap_id = toupper(trap_id),
        method = case_when(
          str_detect(tolower(trap_id), "gr") ~ "G",
          TRUE ~ "L"
        )
      )
  }
  
  # Standardize species names
  if ("spp" %in% names(df)) {
    df <- df %>%
      mutate(
        spp = case_when(
          spp == "Culex tarsalis" ~ "Tarsalis",
          spp == "Culex pipiens" ~ "Pipiens",
          TRUE ~ spp
        )
      )
  }
  
  # Convert total count to numeric
  if ("total" %in% names(df)) {
    df <- df %>%
      mutate(total = as.numeric(total))
  }
  
  return(df)
}
