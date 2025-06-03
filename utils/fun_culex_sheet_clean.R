#' Clean a Culex Surveillance Data Sheet
#'
#' Processes and standardizes raw Culex mosquito surveillance data.
#' Trims whitespace from character columns, parses collection dates using
#' `parse_flexible_date()`, assigns species and method categories, and
#' returns a cleaned data frame with selected columns.
#'
#' @param df A data frame containing raw Culex surveillance data. Must include the columns:
#' `trap_name`, `date_trap_set`, `mosquito_species`, `trap_type`, `mosquito_count`, and `zone`.
#'
#' @return A cleaned and standardized data frame with columns:
#' `trap_id`, `trap_date`, `year`, `week`, `zone`, `spp`, `method`, and `total`.
#'
#' @details This function depends on `parse_flexible_date()` and `col_rename()` which must be defined elsewhere
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
culex_sheet_clean <- function(df) {
  
   
  col_key <- list(
    trap_id   = c("trap_name"),
    trap_date = c("date_trap_set"),
    spp       = c("mosquito_species"),
    method    = c("trap_type"),
    total     = c("mosquito_count"),
    zone      = c("zone")
  )

  missing_cols <- setdiff(names(col_key), names(df))
  if (length(missing_cols) > 0) {
    Cat(" Warning. Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  df = key_rename(df, col_key, drop = T)
  
  df %>%
    dplyr::mutate(dplyr::across(where(is.character), trimws)) %>% #remove any whitespace
    dplyr::mutate(trap_date = purrr::map_chr(trap_date, ~ as.character(parse_flexible_date(.x)))) %>% #reformat dates using the utils/fun_parse_flexible_date
    dplyr::mutate(trap_date = as.Date(trap_date)) %>%
    dplyr::mutate(
      trap_id = toupper(trap_name),
      trap_date = trap_date,
      year = lubridate::year(trap_date),
      week = lubridate::week(trap_date),
      zone = zone,
      spp = dplyr::case_when(
        spp == "Culex tarsalis" ~ "Tarsalis",
        spp == "Culex pipiens" ~ "Pipiens",
        TRUE ~ spp
      ),
   #   trap_type = trap_type,
      method = case_when(stringr::str_detect(tolower(trap_id), "gr") ~ "G",
                         stringr::str_detect(tolower(trap_id), "gr", negate = T) ~ "L"
                    ),
      total = as.numeric(mosquito_count)
    )
}
