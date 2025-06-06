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
#' @details This function depends on parse_flexible_date() which must be defined elsewhere
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
clean_summary <- function(df, col_name, label = deparse(substitute(col_name))) {
  # Ensure tidy evaluation of column
  col <- rlang::enquo(col_name)
  
  # Count NA and non-NA rows
  na <- df %>% filter(is.na(!!col)) %>% nrow()
  matched <- df %>% filter(!is.na(!!col)) %>% nrow()
  
  # Print result
  cat("\nFor", label, ":", matched, "rows matched, and", na, "are NA.\n")
}

#' @export
wnv_s_clean <- function(df, 
                        cols_2_clean = c("trap_id", "zone", "zone2", 
                                             "trap_date", "year", "week", 
                                             "trap_status", "spp", "method", 
                                             "total"),
                        drop_col = NULL
                        ) {
  
  cols_2_clean = setdiff(cols_2_clean, drop_col)
  
  # CHECK COLS ----------------------------------------------------------
  present_cols <- intersect(cols_2_clean, names(df))
  missing_cols <- setdiff(cols_2_clean, names(df))
  
  if (length(missing_cols) > 0) {
    cat("\n Warning following are not present for cleaning: ", paste(missing_cols, collapse = ", "), "\n")
    cat("Run key_rename to convert columns to standard naming convention.")
  }
  
  if (length(present_cols) > 0) {
    cat("\n The Following columns are being cleaned: ", paste(present_cols, collapse = ", "), "\n")
  }
  
  # Trim whitespace from all character columns
  df <- df %>%
    mutate(across(where(is.character), trimws))
  
  
  # CLEAN ZONE ---------------------------------------------------------------
  if("zone" %in% names(df) & "zone" %in% cols_2_clean) {
    
    #fix Berthoud to BE
    df <- df %>%
      mutate(zone = if_else(str_detect(zone, "Berthoud", ), "BE", zone))
    
    # Create regex pattern like "NE|NW|SE|SW|LV|BE|BC"
    valid_zones <- c("NE", "NW", "SE", "SW", "LV", "BE", "BC")
    zone_pattern <- str_c(valid_zones, collapse = "|")
    
    df <- df %>%
      mutate(zone = str_extract(zone, zone_pattern))
    
    clean_summary(df, zone) 
   
  }

  # CLEAN ZONE2 ---------------------------------------------------------------  
  if("zone" %in% names(df) & "zone2" %in% cols_2_clean) {
    df <- df %>%
      mutate(zone2 = if_else(zone %in% c("NE", "NW", "SE", "SW"), "FC", zone))
    cat("\nCreated column zone2\n")
  }

  
  #CLEAN DATE ----------------------------------------------------------------------------------------------
  if ("trap_date" %in% names(df) & "trap_date" %in% cols_2_clean) {
    if (!exists("parse_flexible_date", mode = "function")) {
      stop("Missing function `parse_flexible_date()`. This is a personal utility function you need to define or import (e.g., from utils/mod-dates.r).")
    }
    
    df <- df %>%
      mutate(
        trap_date = purrr::map_chr(trap_date, ~ as.character(parse_flexible_date(.x))),
        trap_date = as.Date(trap_date)
      )
    
   clean_summary(df, trap_date) 
  }
  
  # Derive year and week from trap_date
  if ("trap_date" %in% names(df) & "year" %in% cols_2_clean & "week" %in% cols_2_clean) {
    df <- df %>%
      mutate(
        year = lubridate::year(trap_date),
        week = lubridate::week(trap_date)
      )
    cat("\nCreated columns year and week \n")
  }
  
  # Standardize trap_id and assign method
  
  #CLEAN METHOD
  if ("trap_id" %in% names(df) & "method" %in% cols_2_clean) {
    df <- df %>%
      mutate(
        trap_id = toupper(trap_id),
        method = case_when(
          str_detect(tolower(trap_id), "gr") ~ "G",
          TRUE ~ "L"
        )
      )
  }
  
  
  # CREATE TRAP_STATUS -------------------------------------------------------------
  if ("spp" %in% names(df) & "trap_status" %in% cols_2_clean) {

    # Define grouped status logic
    df <- df %>%
      group_by(trap_id, trap_date) %>%
      mutate(
        trap_status = case_when(
          any(str_detect(spp, "(?i)malfunction|stolen")) ~ "malfunction",
          any(str_detect(spp, "(?i)no mosquitoes")) ~ "no mosquitoes",
          any(str_detect(spp, "(?i)tarsalis|pipiens")) ~ "culex",
          TRUE ~ "no culex"
                                )
        ) %>%
      ungroup()

    cat("\nCreated column trap_status\n")
  }
  
  # CLEAN SPP --------------------------------------------------------------------
  if ("spp" %in% names(df) & "spp" %in% cols_2_clean) {
    df <- df %>%
      mutate(spp0 = spp) %>% #save original spp
      mutate(spp = case_when(
        str_detect(spp, "(?i)Tarsalis") ~ "Tarsalis",
        str_detect(spp, "(?i)Pipiens") ~ "Pipiens",
        str_detect(spp, "(?i)malfunction|stolen|no mosquitoes") ~ "none", #keep as NA because no spp present
        TRUE ~ "other" #all other spp get grouped as other
                            ) # end case_when
            ) #end mutate
    
    
    clean_summary(df, spp) 
  }
  
  
  # CLEAN TOTAL -------------------------------------------------------------
  if ("total" %in% names(df) & "total" %in% cols_2_clean) {
    df <- df %>%
      mutate(total = as.numeric(total))
  }
  
    df <- df %>%
      select(any_of(cols_2_clean), everything()) %>%
      arrange(desc(trap_date), trap_id)

  cat("\nFinal dataframe contains ", ncol(df), "columns and ", nrow(df), " rows.\n")
  
  return(df)
}
