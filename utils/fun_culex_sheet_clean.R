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
#' @details This function depends on `parse_flexible_date()`, which must be defined elsewhere
#' in the package or user's environment.
#'
#' @examples
#' \dontrun{
#' clean_df <- culex_sheet_clean(raw_df)
#' }
#'
#' @importFrom dplyr mutate across transmute case_when
#' @importFrom purrr map_chr
#' @importFrom lubridate year week
#' @export
culex_sheet_clean <- function(df) {
  required_cols <- c("trap_name", "date_trap_set", "mosquito_species",
                     "trap_type", "mosquito_count", "zone")
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  df %>%
    dplyr::mutate(dplyr::across(where(is.character), trimws)) %>%
    dplyr::mutate(trap_date = purrr::map_chr(date_trap_set, ~ as.character(parse_flexible_date(.x)))) %>%
    dplyr::mutate(trap_date = as.Date(trap_date)) %>%
    dplyr::transmute(
      trap_id = trap_name,
      trap_date = trap_date,
      year = lubridate::year(trap_date),
      week = lubridate::week(trap_date),
      zone = zone,
      spp = dplyr::case_when(
        mosquito_species == "Culex tarsalis" ~ "Tarsalis",
        mosquito_species == "Culex pipiens" ~ "Pipiens",
        TRUE ~ mosquito_species
      ),
      method = dplyr::case_when(
        trap_type == "Gravid Trap" ~ "G",
        trap_type == "CDC Light Trap" ~ "L",
        TRUE ~ trap_type
      ),
      total = as.numeric(mosquito_count)
    )
}
