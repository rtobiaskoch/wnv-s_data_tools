# sandbox/check_unmerged_trap_names.R
#
# Audit trap_ids that were dropped by filter_culex_sheet()'s active-trap
# semi_join (written to 2_mid/dropped_by_active_filter.csv by the pipeline).
#
# Why this file and NOT 2_mid/unmerged_data.csv:
#   unmerged_data.csv captures rows whose trap_id IS in foco_trap but whose
#   (trap_id, year, week, spp) key still failed to merge into the skeleton —
#   typically date/season issues. Diverged or typo'd trap names (e.g.
#   "FC-033075" from the CFC 2006-2017 source) never reach unmerged because
#   filter_culex_sheet() drops them one step earlier via semi_join against
#   trap_keep_df. dropped_by_active_filter.csv preserves those silently-
#   dropped rows so they can be audited here.
#
# Each unique trap_id in the dropped set falls into one of two categories:
#   1) Name exists in foco_trap but active != 1 — expected drop, ignore.
#   2) Name absent from foco_trap entirely — diverged / typo candidate.
#
# This script keeps category (2) and shows a Levenshtein-nearest neighbour
# from foco_trap so off-by-one typos are obvious at a glance.

library(dplyr)
library(readr)

# Default input paths — relative to project root.
DEFAULT_DROPPED   <- "2_mid/dropped_by_active_filter.csv"
DEFAULT_FOCO_TRAP <- "1_input/foco_trap - data.csv"

#' Audit diverged trap names against the foco_trap reference.
#'
#' @param fn_dropped   Path to dropped_by_active_filter.csv from the pipeline.
#' @param fn_foco_trap Path to foco_trap reference csv.
#' @param drop_inactive Logical. If TRUE (default) drop trap_ids that DO exist
#'   in foco_trap but are inactive — those are expected drops, not typos.
#'
#' @return A tibble, one row per unique trap_id in the dropped file.
audit_dropped_traps <- function(
  fn_dropped    = DEFAULT_DROPPED,
  fn_foco_trap  = DEFAULT_FOCO_TRAP,
  drop_inactive = TRUE
) {
  dropped   <- readr::read_csv(fn_dropped,   show_col_types = FALSE)
  foco_trap <- readr::read_csv(fn_foco_trap, show_col_types = FALSE)

  # Collapse to one row per trap_id so each suspect name appears once with
  # enough context to triage (which source introduced it, how many rows,
  # year range).
  trap_audit <- dropped %>%
    dplyr::group_by(trap_id) %>%
    dplyr::summarise(
      n_dropped_rows = dplyr::n(),
      sources        = paste(sort(unique(source)), collapse = ", "),
      year_min       = suppressWarnings(min(year, na.rm = TRUE)),
      year_max       = suppressWarnings(max(year, na.rm = TRUE)),
      .groups = "drop"
    )

  # Join the full foco_trap (NOT pre-filtered to active==1) to distinguish:
  #   - active=NA  -> not in foco_trap at all (diverged name, real suspect)
  #   - active=0   -> in foco_trap but inactive (expected drop)
  #   - active=1   -> impossible here; would not have been dropped upstream
  trap_audit <- trap_audit %>%
    dplyr::left_join(
      foco_trap %>% dplyr::select(trap_id, active),
      by = "trap_id"
    ) %>%
    dplyr::mutate(
      in_foco_trap = !is.na(active),
      active       = dplyr::if_else(in_foco_trap, as.logical(active), NA),
      category     = dplyr::case_when(
        !in_foco_trap                 ~ "diverged_name",
        in_foco_trap & active == FALSE ~ "inactive_known",
        TRUE                           ~ "unexpected"
      )
    )

  # Suggest nearest foco_trap name via Levenshtein distance (base R adist).
  # A nearest_dist of 1-2 is almost certainly a typo; >= 4 is a different name.
  ref_names <- foco_trap$trap_id
  dist_mat  <- utils::adist(trap_audit$trap_id, ref_names)
  nearest_idx <- max.col(-dist_mat, ties.method = "first")
  trap_audit <- trap_audit %>%
    dplyr::mutate(
      nearest_foco_trap = ref_names[nearest_idx],
      nearest_dist      = dist_mat[cbind(seq_len(nrow(.)), nearest_idx)]
    )

  # Default: drop the inactive-known category. Those are not name problems —
  # they are traps that exist in foco_trap but were retired or not yet active.
  if (drop_inactive) {
    trap_audit <- trap_audit %>% dplyr::filter(category == "diverged_name")
  }

  trap_audit %>% dplyr::arrange(dplyr::desc(n_dropped_rows))
}

# ---- Run ----------------------------------------------------------------
diverged <- audit_dropped_traps()

cli::cli_h1("Diverged trap-name audit")
cli::cli_alert_info("Suspect trap_ids (no match in foco_trap): {nrow(diverged)}")
cli::cli_alert_info("Total dropped rows under those names: {sum(diverged$n_dropped_rows)}")

print(diverged, n = Inf)
