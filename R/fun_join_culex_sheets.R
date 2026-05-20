# join_merge_z: Full-join two culex data sources, coalescing shared columns.
#
# Priority rule: df_z1 (culex_sheet) values always take precedence over df_z2
# (database) values. The database supplements only — it contributes rows for
# trap-weeks absent from the culex sheet (e.g., CDC Berthoud 2024 traps set
# before VDCI took over the contract).
#
# The join key is fixed to (trap_id, spp, year, week) — the biological unit of
# observation. All other shared columns are resolved by coalescing .x (culex_sheet)
# over .y (database):
#   - row exists in both   -> culex_sheet value wins
#   - culex_sheet only     -> culex_sheet value retained
#   - database only        -> database value used (no alternative)
#
# The `check` flag surfaces rows where both sources had a total count but
# disagreed — useful for QC review after joining.
#
# @param df_z1     Primary data frame (culex sheet; higher priority)
# @param df_z2     Supplementary data frame (database; lower priority)
# @param key_cols  Join key columns — must uniquely identify one observation.
#                  Default: trap_id, spp, year, week.
# @return          Data frame with one row per (trap_id, spp, year, week).
#                  Columns: all coalesced shared columns, `source`, `check`, `key`.

join_cxsheet_database = function(
  df_z1,
  df_z2,
  key_cols = c("trap_id", "spp", "year", "week")
) {
  # Columns present in both data frames that are NOT join keys.
  # After full_join these will gain .x/.y suffixes (e.g. total.x, total.y)
  # and need to be coalesced into a single column.
  shared_cols = setdiff(intersect(colnames(df_z1), colnames(df_z2)), key_cols)

  # Full join on the explicit biological key only.
  # suffix = c(".x", ".y") tags shared non-key columns so they can be
  # coalesced below. Using an explicit key (not dynamic intersection) prevents
  # columns like `source` — which differ between the two inputs — from entering
  # the join key and breaking all matches.
  joined = dplyr::full_join(df_z1, df_z2, by = key_cols, suffix = c(".x", ".y"))

  # Coalesce each shared non-key column pair (.x / .y) into a single column.
  #
  # purrr::reduce threads the data frame through one mutate per shared column,
  # accumulating the result. The anonymous function receives:
  #   df  — the data frame so far (starts as `joined`, grows each iteration)
  #   col — the next column name string from shared_cols
  #
  # Inside mutate:
  #   !!col :=           tidy-eval: assign to a column named by the variable `col`
  #   .data[[...]]       pronoun-safe way to access a column by computed string name
  #   coalesce(.x, .y)   returns the first non-NA value — .x (culex_sheet) is always
  #                      listed first, so culex_sheet values win when both are present.
  coalesced = purrr::reduce(
    shared_cols,
    function(df, col) {
      dplyr::mutate(
        df,
        !!col := dplyr::coalesce(
          .data[[paste0(col, ".x")]], # culex_sheet value (preferred)
          .data[[paste0(col, ".y")]] # database value (fallback when .x is NA)
        )
      )
    },
    .init = joined
  )

  coalesced %>%
    dplyr::mutate(
      # Flag rows where both sources reported a total but disagreed.
      # Arrange desc(check) so discrepancies surface at the top for review.
      tl_match = !is.na(total.x) & !is.na(total.y) & total.x != total.y,
      # Composite key used downstream for deduplication checks (get_dupes).
    ) %>%
    # Drop the .x/.y staging columns — all information is now in the coalesced
    # columns created above by the reduce loop.
    dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y")) %>%
    dplyr::arrange(dplyr::desc(tl_match))
}
