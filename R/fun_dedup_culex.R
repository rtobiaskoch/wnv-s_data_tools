# culex_dedup: Resolve within-week collection date near-duplicates.
#
# Some VDCI weekly files contain two collection events for the same trap within
# the same ISO week — one on a weekday (routine surveillance) and one on a
# weekend (field test or aberrant run). These near-duplicates share the same
# (trap_id, spp, year, week) key but differ in trap_date, so distinct_all()
# does not remove them. Downstream they create duplicate keys when joined
# against the wnv-s_database (which aggregates to one row per trap-week).
#
# Deduplication logic (conservative):
#   - Single-date trap-weeks are kept unchanged — no weekend-based removal.
#   - Multi-date trap-weeks that include a weekend date: keep the collection
#     date with the highest total mosquito count summed across all species.
#     Ties favour weekday dates; remaining ties favour the later date.
#     Exactly one date is always kept (no ties retained).
#
# Must be called AFTER wnv_s_clean() so that trap_date is class Date and
# year/week have been derived from it.
#
# @param df        Data frame with columns matching key_cols plus trap_date and total.
# @param key_cols  Character vector of columns that uniquely identify a trap-week.
#                  Passed to make_key() — must contain at least two names.
# @return          Data frame with at most one collection date per near-duplicate
#                  trap-week; single-date trap-weeks are returned unchanged.

culex_dedup = function(df, key_cols = c("trap_id", "year", "week")) {

  required = c(key_cols, "trap_date", "total")
  missing  = setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("culex_dedup: missing columns: ", paste(missing, collapse = ", "),
         " — run wnv_s_clean() first.")
  }
  if (!inherits(df$trap_date, "Date")) {
    stop("culex_dedup: `trap_date` must be class Date — run wnv_s_clean() first.")
  }

  # Pass 1: remove completely identical rows before any key-based logic.
  # These arise when the same file is read twice or two input sources overlap exactly.
  n_exact_before <- nrow(df)
  df <- dplyr::distinct(df)
  n_exact_removed <- n_exact_before - nrow(df)
  if (n_exact_removed > 0) {
    cat("\nculex_dedup: removed", n_exact_removed, "exact duplicate rows.\n")
  }

  # Pass 2: same-key same-date conflicts with different totals → keep max total.
  # distinct() above only removes rows identical in ALL columns; these rows
  # share (trap_id, spp, year, week, trap_date) but differ in total, so they
  # slip through. Grouping by every column except total and slicing max ensures
  # exactly one row per unique non-total combination.
  n_conflict_before <- nrow(df)
  df <- df %>%
    dplyr::group_by(dplyr::across(-total)) %>%
    dplyr::slice_max(total, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  n_conflict_removed <- n_conflict_before - nrow(df)
  if (n_conflict_removed > 0) {
    cat("\nculex_dedup: resolved", n_conflict_removed,
        "same-date same-key conflicts by keeping max total.\n")
  }

  # Build a single composite key column for all grouping and joining below.
  # make_key() strips non-alphanumeric chars from values; "|" sep is preserved.
  df <- make_key(df, key_cols, name = "dedup_key")

  # Identify trap-weeks that have more than one distinct collection date AND
  # at least one of those dates falls on a weekend (wday 1 = Sunday, 7 = Saturday).
  # Single-date trap-weeks — including those collected on a weekend — are excluded
  # so that legitimate lone-weekend collections are never removed.
  near_dup_keys <- df %>%
    dplyr::group_by(dedup_key) %>%
    dplyr::summarise(
      n_dates  = dplyr::n_distinct(trap_date),
      has_wknd = any(lubridate::wday(trap_date) %in% c(1L, 7L), na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    dplyr::filter(n_dates > 1, has_wknd) %>%
    dplyr::select(dedup_key)

  n_near_dups <- nrow(near_dup_keys)

  if (n_near_dups == 0) {
    cat("\nculex_dedup: no near-duplicate trap-weeks found. No rows removed.\n")
    return(dplyr::select(df, -dedup_key))
  }

  # For each near-duplicate trap-week, select the single collection date with the
  # highest summed total across all species (the most informative night).
  # Tie-breaking order:
  #   1. Highest total (desc)
  #   2. Weekday preferred over weekend (is_wknd FALSE sorts before TRUE)
  #   3. Later date preferred (desc trap_date)
  # slice_head(n = 1) always returns exactly one row per group — no ties retained.
  best_dates <- df %>%
    dplyr::semi_join(near_dup_keys, by = "dedup_key") %>%
    dplyr::group_by(dedup_key, trap_date) %>%
    dplyr::summarise(total_sum = sum(total, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(is_wknd = lubridate::wday(trap_date) %in% c(1L, 7L)) %>%
    dplyr::group_by(dedup_key) %>%
    dplyr::arrange(
      dplyr::desc(total_sum),  # most mosquitoes first
      is_wknd,                 # FALSE (weekday) before TRUE (weekend) for ties
      dplyr::desc(trap_date),  # later date first for remaining ties
      .by_group = TRUE
    ) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(dedup_key, trap_date)

  # Reconstruct the output:
  #   - Single-date trap-weeks: pass through unchanged
  #   - Near-duplicate trap-weeks: keep only rows from the best collection date
  n_before <- nrow(df)

  df_single <- dplyr::anti_join(df, near_dup_keys, by = "dedup_key")
  df_best   <- dplyr::semi_join(df, best_dates,    by = c("dedup_key", "trap_date"))
  df_out    <- dplyr::bind_rows(df_single, df_best) %>%
    dplyr::select(-dedup_key)

  n_removed <- n_before - nrow(df_out)

  cat(
    "\nculex_dedup:", n_near_dups,
    "trap-weeks had multiple collection dates including a weekend.\n",
    " Kept highest-count date per trap-week; removed", n_removed,
    "rows.", nrow(df_out), "rows remaining.\n"
  )

  df_out
}
