library(dplyr)

#' Fill Skeleton from Multiple Data Sources in Priority Order
#'
#' Joins each source data frame into the skeleton in the order provided.
#' The first source fills as many skeleton cells as possible; each subsequent
#' source only patches cells still NA after all higher-priority sources have run.
#'
#' This preserves the skeleton as the deduplication boundary: a record from a
#' higher-priority source always wins, and lower-priority sources never overwrite
#' existing values. Adding a new data source is a single list entry — no join
#' logic needs to change elsewhere in the pipeline.
#'
#' @param skeleton  Data frame from expand_trap(). Must have a `key` column and
#'   no data columns yet (trap_date, trap_status, total, source will be added).
#' @param sources   Named list of data frames in priority order (highest first).
#'   Names become the value of the `source` column in the output so sources are
#'   traceable in the final dataset. Each data frame must contain:
#'   key (character), trap_date (Date), trap_status (character), total (numeric).
#'
#' @return Skeleton data frame with trap_date, trap_status, total, source filled
#'   where matching records exist across the priority sources.
#' @export
fill_skeleton <- function(skeleton, sources) {
  required_cols <- c("key", "trap_date", "trap_status", "total")

  # Validate each source has the required columns before any joins
  purrr::iwalk(sources, function(df, name) {
    missing <- setdiff(required_cols, names(df))
    if (length(missing) > 0) {
      stop(
        "Source '", name, "' is missing required columns: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
  })

  # Initialise data columns as NA so the is.na(trap_status) filter works from
  # the first iteration. The skeleton itself only carries structural columns
  # (key, trap_id, zone, zone2, method, year, week, spp).
  result <- skeleton %>%
    dplyr::mutate(
      trap_date   = as.Date(NA_character_),
      trap_status = NA_character_,
      total       = NA_real_,
      source      = NA_character_
    )

  for (source_name in names(sources)) {
    df <- sources[[source_name]]

    # Only patch skeleton rows where trap_status is still NA — these are rows
    # that no higher-priority source has filled yet. This prevents lower-priority
    # sources from overwriting existing values, and avoids patching malfunction
    # rows where total is legitimately NA but trap_status is already set.
    empty_keys <- result %>%
      dplyr::filter(is.na(trap_status)) %>%
      dplyr::select(key)

    patch <- df %>%
      dplyr::mutate(source = source_name) %>%
      dplyr::select(dplyr::all_of(c(required_cols, "source"))) %>%
      dplyr::semi_join(empty_keys, by = "key") %>%
      # Deduplicate within source: if the source itself has duplicate keys,
      # keep the first occurrence so rows_patch() receives unique keys.
      dplyr::distinct(key, .keep_all = TRUE)

    result <- result %>%
      dplyr::rows_patch(patch, by = "key", unmatched = "ignore")
  }

  result
}


#' Collect Unmerged Records from Each Source
#'
#' For each source in the priority list, returns records whose key is absent
#' from the skeleton. These flag traps that were observed but are not in the
#' active trap reference list. The output is a single data frame with a
#' `source` column identifying the originating data source.
#'
#' @param skeleton  Data frame from expand_trap(). Must have a `key` column.
#' @param sources   Named list of data frames (same as passed to fill_skeleton()).
#'   Each must contain a `key` column.
#'
#' @return Data frame of unmerged records with a `source` column added.
#' @export
collect_unmerged <- function(skeleton, sources) {
  purrr::imap_dfr(sources, function(df, source_name) {
    df %>%
      dplyr::anti_join(skeleton, by = "key") %>%
      dplyr::mutate(source = source_name)
  })
}
