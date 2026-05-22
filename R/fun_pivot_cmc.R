#' Pivot CMC Weekly Sheets to Long Format
#'
#' CMC 2015-2018 weekly files are wide: one column per species
#' (`Cx tarsalis`, `Cx pipiens`). The downstream chain expects one row per
#' (trap, species) with `mosquito_species` and `mosquito_count` columns.
#'
#' This function:
#'   1. Renames `Trap Number` -> `trap_name` and `Trap Date` -> `date_trap_set`
#'      so key_rename() maps them to the canonical names.
#'   2. Pivots the two species columns to long format.
#'   3. Synthesizes a "malfunction" species row for any trap-date where
#'      `Malfunction` is not "NO" so wnv_s_clean()'s trap_status logic fires.
#'
#' Column casing is inconsistent across years; the function normalises
#' multi-whitespace before matching.
#'
#' @param df Raw imported CMC weekly sheet.
#' @return Long-format tibble with trap_name, date_trap_set, Zone,
#'   mosquito_species, mosquito_count, Week, and other pass-through columns.
#' @export
pivot_cmc <- function(df) {
  names(df) <- stringr::str_replace_all(names(df), "\\s+", " ")

  # Some CMC files (e.g. 2018 Week 34) have a company/title row before the
  # real headers. Detect by looking for "Week" in the first column's values.
  if (!"Week" %in% names(df) && any(df[[1]] == "Week", na.rm = TRUE)) {
    header_row <- which(df[[1]] == "Week")[1]
    col_names  <- as.character(unlist(df[header_row, ]))
    col_names  <- stringr::str_replace_all(col_names, "\\s+", " ")
    df         <- df[(header_row + 1):nrow(df), ]
    names(df)  <- col_names
    names(df)  <- stringr::str_replace_all(names(df), "\\s+", " ")
  }

  spp_cols <- intersect(
    c("Cx tarsalis", "Cx pipiens", "Cx Tarsalis", "Cx Pipiens"),
    names(df)
  )
  if (length(spp_cols) == 0L) {
    stop("pivot_cmc: no Cx tarsalis / Cx pipiens columns found. Columns: ",
         paste(names(df), collapse = ", "))
  }

  long <- df %>%
    dplyr::rename(
      trap_name     = dplyr::any_of("Trap Number"),
      date_trap_set = dplyr::any_of("Trap Date")
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(spp_cols),
      names_to  = "mosquito_species",
      values_to = "mosquito_count"
    ) %>%
    # Normalise mosquito_count to character so bind_rows with the malf row
    # is type-safe regardless of whether the source imported it as numeric
    # or character (varies by file vintage).
    dplyr::mutate(mosquito_count = as.character(mosquito_count))

  # Synthesize a malfunction row per trap-date where Malfunction is not "NO".
  # wnv_s_clean() detects "malfunction" in spp0 to set trap_status.
  if ("Malfunction" %in% names(long)) {
    malf <- long %>%
      dplyr::filter(
        !is.na(Malfunction),
        !stringr::str_detect(Malfunction, "(?i)^no$|^n$")
      ) %>%
      dplyr::distinct(trap_name, date_trap_set, .keep_all = TRUE) %>%
      dplyr::mutate(
        mosquito_species = "malfunction",
        mosquito_count   = NA_character_
      )
    long <- dplyr::bind_rows(long, malf)
  }

  long
}
