library(dplyr)

#' Resolve Trap Status for Unmatched Skeleton Rows After Join
#'
#' After expand_trap() skeleton is left-joined to cleaned culex data, rows
#' with no match have trap_status = NA. This function assigns status by
#' inspecting whether the zone-week had any real observation:
#'
#'   - Zone-week has >=1 non-NA trap_status row -> NA rows become "no mosquitoes"
#'     (trap was set; source omits zero-catch rows by convention).
#'   - Zone-week has zero non-NA rows -> all rows become "no trap"
#'     (no trapping occurred in that zone-week).
#'
#' Existing trap_status values (culex, no mosquitoes, malfunction, no culex)
#' are always preserved. Only NA rows are updated.
#'
#' @param df  Post-join expanded data frame. Must contain: zone (character),
#'   year (integer), week (integer), trap_status (character, NA where unmatched),
#'   total (numeric or NA). Type consistency on zone/year/week is required for
#'   the zone-week join to match correctly.
#'
#' @return Data frame with trap_status and total resolved for all rows.
#' @export
assign_trap_status_post_expand <- function(df) {

  # Identify zone-weeks where at least one trap has a real (non-NA) observation.
  # A "malfunction" row counts as having data — the zone was active that week.
  zone_weeks_with_data <- df %>%
    dplyr::filter(!is.na(trap_status)) %>%
    dplyr::distinct(zone, year, week) %>%
    dplyr::mutate(.has_data = TRUE)

  df %>%
    # Join flag indicating whether any trap in this zone-week reported data
    dplyr::left_join(zone_weeks_with_data, by = c("zone", "year", "week")) %>%
    dplyr::mutate(
      # Resolve trap_status: preserve existing, then infer from zone-week context
      trap_status = dplyr::case_when(
        !is.na(trap_status) ~ trap_status,      # preserve all real statuses
        !is.na(.has_data)   ~ "no mosquitoes",  # zone active; trap absent from source
        TRUE                ~ "no trap"          # whole zone-week absent from data
      ),
      # Resolve total: no trap = NA, no mosquitoes = always 0, else keep as-is
      total = dplyr::case_when(
        trap_status == "no trap"       ~ NA_real_,
        # "no mosquitoes" means the trap ran but caught nothing — total is always 0
        # regardless of what the source provided. Any non-zero value here is a
        # data error (contradicts the status label).
        trap_status == "no mosquitoes" ~ 0,
        TRUE                           ~ as.numeric(total)
      )
    ) %>%
    dplyr::select(-.has_data)
}
