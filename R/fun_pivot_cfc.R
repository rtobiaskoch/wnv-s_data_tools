#' Reshape CFC 2006-2017 Consolidated Sheet to VDCI Long Format
#'
#' The CFC CombinedData sheet is already one row per (trap, date, species) —
#' no pivot is needed. This function only renames CFC-specific column names
#' to the VDCI-compatible intermediate names that key_rename() then maps
#' to canonical pipeline names (trap_id, trap_date, etc.).
#'
#' Column mapping:
#'   Collection Site -> trap_name  (key_rename: trap_id,trap_name)
#'   Date            -> date_trap_set  (key_rename: trap_date,date_trap_set)
#'   MMWR Week       -> Week       (key_rename: week,Week)
#'   Species         -> mosquito_species (key_rename: spp,mosquito_species)
#'   Total           -> mosquito_count  (key_rename: total,mosquito_count)
#'   Zone            -> stays Zone  (key_rename: zone,Zone)
#'   Year            -> stays Year  (key_rename: year,Year)
#'
#' Extra CFC-only columns (ID, TestCode, PIR-*) are dropped by
#' key_rename(drop_extra = TRUE) in the main pipeline.
#'
#' @param df Raw imported CFC CombinedData sheet.
#' @return Long-format tibble with VDCI-compatible intermediate column names.
#' @export
pivot_cfc <- function(df) {
  # Normalise multi-space column names
  names(df) <- stringr::str_replace_all(names(df), "\\s+", " ")

  # Rename CFC-specific columns to VDCI intermediate names.
  # any_of() silently skips absent variants so the function is robust to
  # minor header drift across CFC file versions.
  df <- df %>%
    dplyr::rename(
      trap_name        = dplyr::any_of("Collection Site"),
      date_trap_set    = dplyr::any_of("Date"),
      Week             = dplyr::any_of("MMWR Week"),
      mosquito_species = dplyr::any_of("Species"),
      mosquito_count   = dplyr::any_of("Total")
    )

  # CFC dates arrive from rio::import() as Excel serial numerics (days since
  # 1899-12-30, e.g. 38875 = 2006-06-23). The downstream as.character() coercion
  # in read_source() would otherwise turn them into the string "38875", which
  # parse_flexible_date() cannot parse — silently zeroing year/week and
  # preventing CFC keys from matching the skeleton. Convert to Date here so
  # the character coercion produces an ISO date string ("2006-06-23") that
  # parse_flexible_date() handles natively.
  if ("date_trap_set" %in% names(df) && is.numeric(df$date_trap_set)) {
    df <- df %>%
      dplyr::mutate(date_trap_set = as.Date(date_trap_set, origin = "1899-12-30"))
  }

  df
}
