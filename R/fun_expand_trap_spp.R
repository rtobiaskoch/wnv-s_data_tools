library(dplyr)
library(tidyr)
library(lubridate)

#' Expand Trap Grid by Year, Week, and Species
#'
#' Builds a fully-crossed expansion of the active trap list across all years,
#' trapping weeks, and species, then left-joins to culex_database to fill in
#' observed counts. Returns a dataframe ready to pass to wnv_s_clean().
#'
#' The join is at the trap-week level (trap_id, zone, year, week) so that
#' "none" spp rows in culex_database (malfunction, no mosquitoes) are
#' preserved as-is and carry their trap_status signal into wnv_s_clean().
#' The spp completion step runs only for culex rows, adding a 0-total row
#' for any species missing from a trap-week where the other species was caught.
#'
#' Zone-weeks absent from culex_database entirely are flagged as
#' trap_status = "No Traps" with trap_date = NA (year and week remain populated).
#'
#' @param trap_keep_df Data frame from fn_trap_keep filtered to active traps.
#'   Must contain: trap_id, zone, start (first active year).
#' @param culex_database Data frame from join_cxsheet_database(). Must contain:
#'   trap_id, spp, zone, year, week, total, trap_date, trap_status.
#' @param spp_levels Character vector of culex species to expand over.
#'   Typically c("Tarsalis", "Pipiens") from cfg$spp_levels.
#'
#' @return Data frame with one row per trap × year × week × relevant spp,
#'   ready to pass to wnv_s_clean().
#'
#' @importFrom dplyr mutate select left_join anti_join distinct group_by
#'   ungroup if_else filter bind_rows
#' @importFrom tidyr crossing fill
#' @importFrom lubridate year
#' @export
expand_trap_spp <- function(trap_keep_df, culex_database, spp_levels) {

  y <- lubridate::year(Sys.Date())

  # ── Step 1: Expand trap × year × week ───────────────────────────────────────
  # Each trap has its own start year so rowwise_expand preserves per-trap ranges.
  trap_expand <- trap_keep_df %>%
    dplyr::mutate(
      end     = y,
      w_start = 23L,
      w_end   = 37L
    ) %>%
    rowwise_expand("start", "end",     "year") %>%
    rowwise_expand("w_start", "w_end", "week") %>%
    dplyr::select(trap_id, zone, year, week)

  # ── Step 2: Identify "No Traps" zone-weeks ──────────────────────────────────
  # Zone-weeks absent from culex_database had no trapping at all that week.
  zone_week_present <- culex_database %>%
    dplyr::distinct(zone, year, week)

  no_traps_zone_weeks <- trap_expand %>%
    dplyr::distinct(zone, year, week) %>%
    dplyr::anti_join(zone_week_present, by = c("zone", "year", "week"))

  # ── Step 3: Left join at trap-week level (no spp in join key) ───────────────
  # Joining without spp preserves all rows from culex_database, including
  # "none" spp rows (malfunction, no mosquitoes) that carry trap_status signals
  # for wnv_s_clean() to read. This is why spp must NOT be in the join key.
  culex_joined <- dplyr::left_join(
    trap_expand,
    culex_database,
    by = c("trap_id", "zone", "year", "week")
  )

  # ── Step 4: Split rows into three categories ─────────────────────────────────
  # culex: spp = "Tarsalis"/"Pipiens", trap_status = "culex" → spp completion
  # status: spp = "none"/"non culex" (malfunction, no mosquitoes, non-culex)
  #         → preserved as-is so wnv_s_clean() can read their trap_status
  # no_data: spp = NA (trap not in culex_database this week) → expand by spp
  culex_rows   <- culex_joined %>% dplyr::filter(trap_status == "culex")
  status_rows  <- culex_joined %>% dplyr::filter(trap_status != "culex" & !is.na(spp))
  no_data_rows <- culex_joined %>% dplyr::filter(is.na(spp))

  # ── Step 5: Complete spp dimension for culex rows ───────────────────────────
  # Adds a 0-total row for any spp missing from a culex trap-week.
  # Example: only Tarsalis was caught → add Pipiens row with total = 0.
  culex_present <- culex_rows %>%
    dplyr::distinct(trap_id, zone, year, week, spp)

  missing_spp <- culex_rows %>%
    dplyr::distinct(trap_id, zone, year, week) %>%
    tidyr::crossing(spp = spp_levels) %>%
    dplyr::anti_join(culex_present,
                     by = c("trap_id", "zone", "year", "week", "spp")) %>%
    dplyr::mutate(total = 0)

  culex_complete <- dplyr::bind_rows(culex_rows, missing_spp)

  # ── Step 6: Expand no-data rows by spp ──────────────────────────────────────
  # Trap-weeks with no culex_database entry get a Tarsalis and Pipiens row
  # (total = 0). wnv_s_clean() classifies these as "no mosquitoes" (zone active)
  # or "No Traps" (zone had no trapping at all, flagged in step 7).
  no_data_spp <- no_data_rows %>%
    dplyr::select(trap_id, zone, year, week) %>%
    tidyr::crossing(spp = spp_levels) %>%
    dplyr::mutate(total = 0)

  # ── Step 7: Combine and flag "No Traps" zone-weeks ──────────────────────────
  # status_rows are bound before wnv_s_clean() so their trap_status is visible
  # to the case_when guards (malfunction, no culex). total is coerced to numeric
  # to ensure consistent types after the three-way bind_rows.
  culex_expanded <- dplyr::bind_rows(culex_complete, status_rows, no_data_spp) %>%
    dplyr::mutate(total = as.numeric(total)) %>%
    dplyr::left_join(
      no_traps_zone_weeks %>% dplyr::mutate(.no_traps = TRUE),
      by = c("zone", "year", "week")
    ) %>%
    dplyr::mutate(
      trap_status = dplyr::if_else(
        !is.na(.no_traps), "No Traps", trap_status, missing = trap_status
      ),
      trap_date   = dplyr::if_else(
        !is.na(.no_traps), as.Date(NA_character_), trap_date
      ),
      total       = dplyr::if_else(
        !is.na(.no_traps), NA_real_, total
      )
    ) %>%
    dplyr::select(-.no_traps)

  # ── Step 8: Propagate trap_date within zone-week ────────────────────────────
  # Fills missing dates from any trap in the same zone-week that had data.
  # "No Traps" zone-weeks are all-NA within the group so fill has no source;
  # the explicit re-clear below is a defensive guard.
  culex_expanded <- culex_expanded %>%
    dplyr::group_by(year, week, zone) %>%
    tidyr::fill(trap_date, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      trap_date = dplyr::if_else(
        trap_status == "No Traps", as.Date(NA_character_), trap_date
      )
    )

  # ── Step 9: Normalize to one date per trap-week ─────────────────────────────
  # Ensures all rows for the same trap-week (including status_rows and 0-fill
  # rows) share a single date so wnv_s_clean() groups them together correctly.
  culex_expanded <- culex_expanded %>%
    dplyr::group_by(trap_id, zone, year, week) %>%
    dplyr::mutate(
      trap_date = dplyr::first(sort(trap_date, na.last = TRUE))
    ) %>%
    dplyr::ungroup()

  return(culex_expanded)
}
