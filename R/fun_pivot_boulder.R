#' Normalise Boulder Culex Sheets to Long Format with Zone = BC
#'
#' Boulder files from 2021 onward use the same long format as VDCI
#' (date_trap_set, zone, mosquito_species, mosquito_count, trap_name).
#' This function is primarily a guard: it forces zone = "BC" on every row
#' (Boulder sometimes omits the column) and passes the data through unchanged.
#'
#' If a future Boulder file uses a wide format (Cx tarsalis / Cx pipiens
#' columns), the wide-to-long branch handles the reshape before the zone
#' enforcement.
#'
#' @param df Raw imported Boulder Culex sheet.
#' @return Long-format tibble with zone (or Zone) = "BC" on every row.
#' @export
pivot_boulder <- function(df) {
  names(df) <- stringr::str_replace_all(names(df), "\\s+", " ")

  # Handle any wide-format variant that may appear in future files
  wide_spp <- intersect(
    c("Cx tarsalis", "Cx pipiens", "Cx Tarsalis", "Cx Pipiens"),
    names(df)
  )

  long <- if (length(wide_spp) > 0L) {
    df %>%
      dplyr::rename(
        trap_name     = dplyr::any_of(c("Trap Number", "trap_name")),
        date_trap_set = dplyr::any_of(c("Trap Date", "date_trap_set"))
      ) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(wide_spp),
        names_to  = "mosquito_species",
        values_to = "mosquito_count"
      )
  } else {
    df
  }

  # Force zone = "BC" regardless of what the sheet says
  if ("zone" %in% names(long)) {
    long$zone <- "BC"
  } else if ("Zone" %in% names(long)) {
    long$Zone <- "BC"
  } else {
    long$zone <- "BC"
  }

  long
}
