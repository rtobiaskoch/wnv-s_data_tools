#' Fix Zone Using Authoritative Trap Reference
#'
#' Overwrites the zone column in df using trap_id as the join key against an
#' authoritative trap reference (e.g., foco_trap - data.csv). Rows whose
#' trap_id is absent from the reference are left unchanged.
#'
#' Use this before wnv_s_clean() so that zone2 is derived from the corrected
#' zone value.
#'
#' @param df Data frame with columns trap_id and zone.
#' @param trap_ref Authoritative trap reference with columns trap_id and zone.
#' @return df with zone corrected from trap_ref, with a console summary.
#'
#' @importFrom dplyr left_join mutate if_else select
#' @export

fix_zone_from_ref <- function(df, trap_ref) {

  # Pull only the authoritative trap_id -> zone mapping
  ref <- trap_ref %>%
    dplyr::select(trap_id, zone_ref = zone) %>%
    dplyr::distinct(trap_id, .keep_all = TRUE)

  df_fixed <- df %>%
    dplyr::left_join(ref, by = "trap_id") %>%
    dplyr::mutate(
      # Use reference zone where available; fall back to original
      zone = dplyr::if_else(!is.na(zone_ref), zone_ref, zone)
    ) %>%
    dplyr::select(-zone_ref)

  n_changed <- sum(df$zone != df_fixed$zone, na.rm = TRUE)
  n_unmatched <- sum(is.na(dplyr::left_join(df, ref, by = "trap_id")$zone_ref))

  cat("\nfix_zone_from_ref:\n",
      n_changed, "zone value(s) corrected\n",
      n_unmatched, "trap_id(s) not found in reference (zone unchanged)\n")

  df_fixed
}
