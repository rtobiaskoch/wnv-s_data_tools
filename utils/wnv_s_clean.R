#' Clean Summary
#' @param df A data frame containing raw Culex surveillance data. Must include the columns:
#' trap_name, date_trap_set, mosquito_species, trap_type, mosquito_count, and zone.
#' @return statement
#' @importFrom dplyr filter
#' @importFrom rlang enquo
#' @export
clean_summary <- function(df0, df, col_name, label = deparse(substitute(col_name))) {
  col <- rlang::enquo(col_name)
  
  # Check if column exists in both dataframes
  col_string <- rlang::as_name(col)
  
  #does new column already exists in original?
  if (col_string %in% names(df0) && col_string %in% names(df)) {
    changed <- sum(dplyr::pull(df0, !!col) != dplyr::pull(df, !!col), na.rm = TRUE)
    unchanged <- sum(dplyr::pull(df0, !!col) == dplyr::pull(df, !!col), na.rm = TRUE)
    # Count NA and non-NA in df
    na0 <- df0 %>% filter(is.na(!!col)) %>% nrow()
    
    
    
  } else { #if not indicate it is being added
    cat("Column", label, "is being added.\n")
    changed <- "NA"
    unchanged <- "NA"
    na0 <- "NA"
    
  }
  na <- df %>% filter(is.na(!!col)) %>% nrow()
  
  # Print summary
  cat("\nFor rows in", label, ":\n",
      changed, "changed,\n",
      unchanged, "unchanged\n",
      na0, " missing in input\n",
      na, " missing in output\n")
}

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

wnv_s_clean <- function(df, 
                        all_cols = c("csu_id", "trap_id", "zone", "zone2", 
                                     "trap_date", "year", "week", 
                                     "spp","spp0", "method", 
                                     "trap_status", "total"),
                        rm_col = c()
                        ) {
  
  #save original input for comparison
  df0 = df
  
  # Check required cleaned columns
  col_2_clean = setdiff(all_cols, rm_col)
  present_cols <- intersect(all_cols, names(df))
  missing_cols <- setdiff(all_cols, names(df))
  
  if (length(missing_cols) > 0) {
    cat("\n Notice. Following are not present for cleaning: ", paste(missing_cols, collapse = ", "), "\n")
    cat("Run key_rename to convert columns to standard naming convention.")
  }
  
  if (length(present_cols) > 0) {
    cat("\n The Following columns are being cleaned: ", paste(present_cols, collapse = ", "), "\n")
  }
  
  # Trim whitespace from all character columns
  df <- df %>%
    mutate(across(where(is.character), trimws))
  
  # CLEAN csu_id
  if("csu_id" %in% names(df) & "csu_id" %in% col_2_clean) {
    
    df <- df %>%
      mutate(csu_id = str_remove(csu_id, "-"))
    
    clean_summary(df0, df, csu_id) 
  }
  
  
  # CLEAN ZONE
  if("zone" %in% names(df) & "zone2" %in% col_2_clean) {
    
    #fix Berthoud to BE
    df <- df %>%
      mutate(zone = if_else(str_detect(zone, "Berthoud", ), "BE", zone))
    
    # Create regex pattern like "NE|NW|SE|SW|LV|BE|BC"
    valid_zones <- c("NE", "NW", "SE", "SW", "LV", "BE", "BC")
    zone_pattern <- str_c(valid_zones, collapse = "|")
    
    df <- df %>%
      mutate(zone = str_extract(zone, zone_pattern))
    
    clean_summary(df0, df, zone) 
  }
  
  
  # CLEAN/GET ZONE2
  if("zone" %in% names(df) & "zone2" %in% col_2_clean) {
    
    # Create regex pattern like "NE|NW|SE|SW|LV|BE|BC"
    fc_zones <- c("NE", "NW", "SE", "SW")
    zone_pattern <- str_c(fc_zones, collapse = "|")
    
    df <- df %>%
      mutate(zone2 = if_else(zone %in% fc_zones, "FC", zone))
    
    clean_summary(df0, df, zone2) 
  }
  
  #CLEAN DATE
  if ("trap_date" %in% names(df) & "trap_date" %in% col_2_clean) {
    if (!exists("parse_flexible_date", mode = "function")) {
    }
    
    df <- df %>%
      mutate(
        trap_date = purrr::map_chr(trap_date, ~ as.character(parse_flexible_date(.x))),
        trap_date = as.Date(trap_date)
      )
    
    clean_summary(df0, df, trap_date) 
  }
  
  # GET YEAR AND WEEK
  if ("trap_date" %in% names(df) & "year" %in% col_2_clean) {
    df <- df %>%
      mutate(
        year = if_else(is.na(year), lubridate::year(trap_date), year),
        week = if_else(is.na(week), lubridate::isoweek(trap_date), week)
            )
    
    clean_summary(df0, df, year) 
    clean_summary(df0, df, week) 
  }
  
  # Standardize trap_id and assign method
  
  #GET METHOD
  if ("trap_id" %in% names(df) & "trap_id" %in% col_2_clean) {
    df <- df %>%
      mutate(
        trap_id = toupper(trap_id),
        method = case_when(
          str_detect(tolower(trap_id), "gr") ~ "G",
          TRUE ~ "L"
        )
      )
    
    clean_summary(df0, df, method) 
  }
  

  
  # CREATE TRAP_STATUS
  if ("spp" %in% names(df) & "trap_status" %in% col_2_clean) {
    
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
    
    clean_summary(df0, df, trap_status) 
    
  }
  
  
  # CLEAN SPP
  if ("spp" %in% names(df) & "spp" %in% col_2_clean) {
    df <- df %>%
      mutate(spp0 = spp,
             spp = case_when(
                      str_detect(spp, "(?i)Tarsalis") ~ "Tarsalis",
                      str_detect(spp, "(?i)Pipiens") ~ "Pipiens",
                      str_detect(spp, "(?i)malfunction|stolen|no mosquitoes") ~ "none",
                      TRUE ~ "non culex"
                           )
          ) 
    clean_summary(df0, df, spp) 
  }
  
  
  # Convert total count to numeric
  if ("total" %in% names(df) & "total" %in% col_2_clean) {
    df <- df %>%
      mutate(total = as.numeric(total))
    
    clean_summary(df0, df, total) 
  }
  
  if("trap_date" %in% names(df) & "trap_id" %in% names(df)) {
    df <- df %>%
      select(any_of(all_cols), everything()) %>%
      arrange(desc(trap_date), trap_id)
  } else {
    df <- df %>%
      select(any_of(all_cols), everything())
  }
  
  return(df)
}
