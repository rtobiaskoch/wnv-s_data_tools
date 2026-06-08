library(dplyr)
library(tidyr)

#' Shape Cleaned Source Data for Skeleton Join
#'
#' Converts cleaned culex surveillance data (which may contain spp values of
#' "Tarsalis", "Pipiens", "none", or "non culex" from wnv_s_clean()) into a
#' form where every row has spp ∈ spp_levels and trap_status is already set.
#'
#' This means fill_skeleton() receives clean, consistently shaped data from any
#' source. No post-hoc propagation of status rows is needed after the join.
#'
#' Four input cases are handled:
#'   "culex":         expand to all spp_levels; missing species gets total = 0
#'   "malfunction":   expand to all spp_levels; total = NA
#'   "no culex":      expand to all spp_levels; total = 0
#'   "no mosquitoes": expand to all spp_levels; total = 0
#'
#' wnv_s_clean() assigns trap_status at the trap-date group level (via
#' group_by(trap_id, trap_date)), so all rows for a trap-date share the same
#' status. The culex vs. status split on trap_status is therefore clean —
#' a trap-date is either entirely "culex" or entirely a status event.
#'
#' @param df        Cleaned data frame from wnv_s_clean(). Must contain:
#'   trap_id, zone, zone2, year, week, trap_date, method, spp, trap_status, total.
#'   zone2 is derived by wnv_s_clean() and is required for downstream plotting.
#' @param spp_levels Character vector of species to expand to (from cfg$spp_levels).
#'
#' @return Data frame with spp ∈ spp_levels only and a rebuilt key column.
#'   Columns: key, trap_id, zone, zone2, year, week, trap_date, method, spp,
#'   trap_status, total.
#' @export
prep_for_skeleton <- function(df, spp_levels) {

  # Split on trap_status: culex catches vs. all status events.
  # All rows in a trap-date group share the same trap_status (wnv_s_clean
  # group assigns), so this split is mutually exclusive.
  culex_rows  <- df %>% dplyr::filter(trap_status == "culex")
  status_rows <- df %>% dplyr::filter(trap_status != "culex")

  # ── culex catches: complete the species dimension ──────────────────────────
  # For each trap-date where culex was caught, produce one row per spp_level.
  # Join actual totals back — missing species (not caught) gets total = 0.
  culex_complete <- if (nrow(culex_rows) > 0L) {
    trap_meta <- culex_rows %>%
      dplyr::distinct(trap_id, zone, zone2, year, week, trap_date, method, trap_status)

    trap_meta %>%
      tidyr::crossing(spp = spp_levels) %>%
      dplyr::left_join(
        culex_rows %>% dplyr::select(trap_id, year, week, spp, total),
        by = c("trap_id", "year", "week", "spp")
      ) %>%
      dplyr::mutate(total = dplyr::coalesce(as.numeric(total), 0))
  } else {
    tibble::tibble()
  }

  # ── status events: expand to all spp_levels ───────────────────────────────
  # malfunction → total = NA (trap did not collect data)
  # no culex / no mosquitoes → total = 0
  status_expanded <- if (nrow(status_rows) > 0L) {
    status_rows %>%
      dplyr::distinct(trap_id, zone, zone2, year, week, trap_date, method, trap_status) %>%
      tidyr::crossing(spp = spp_levels) %>%
      dplyr::mutate(
        total = dplyr::if_else(trap_status == "malfunction", NA_real_, 0)
      )
  } else {
    tibble::tibble()
  }

  dplyr::bind_rows(culex_complete, status_expanded) %>%
    # Rebuild key: spp has changed from "none"/"non culex" to spp_levels values
    wnvSurv::make_key(key_cols = c("trap_id", "spp", "year", "week")) %>%
    # Guard against duplicate keys from the same trap having two dates in one
    # week (culex_dedup should prevent this, but defensive dedup is cheap)
    dplyr::distinct(key, .keep_all = TRUE) %>%
    dplyr::select(key, trap_id, zone, zone2, year, week, trap_date, method,
                  spp, trap_status, total)
}
