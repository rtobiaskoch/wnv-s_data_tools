#' Read All Files Under a Source Root
#'
#' Lists files recursively under `path` matching `pattern`, optionally drops
#' paths containing any string in `exclude_paths`, reads each with rio::import
#' (passing `import_args` for sheet selection etc.), applies an optional
#' per-file pivot function, then row-binds.
#'
#' Replaces read_list() in the multi-source pipeline. Each invocation handles
#' exactly ONE source (one folder root, one pattern, one pivot rule).
#'
#' @param path         Character. Directory to search recursively.
#' @param pattern      Character. Regex passed to list.files().
#' @param pivot_fn     Function or NULL. Applied to each file's imported df
#'   BEFORE binding. Use to reshape wide formats to long.
#' @param exclude_paths Character vector. File paths containing any of these
#'   substrings are dropped (e.g. "all_mosq").
#' @param import_args   Named list. Extra args forwarded to rio::import()
#'   (e.g. list(sheet = "CombinedData") for multi-sheet xlsx).
#'
#' @return A tibble of all rows from all matched files, all columns coerced to
#'   character for consistent downstream handling.
#' @export
read_source <- function(path, pattern, pivot_fn = NULL,
                        exclude_paths = character(),
                        import_args   = list()) {

  files <- list.files(
    path = path, pattern = pattern,
    recursive = TRUE, full.names = TRUE, ignore.case = TRUE
  )

  if (length(exclude_paths) > 0L) {
    drop_re <- paste(exclude_paths, collapse = "|")
    files   <- files[!stringr::str_detect(files, drop_re)]
  }

  if (length(files) == 0L) {
    warning("read_source: no files matched '", pattern, "' under ", path)
    return(tibble::tibble())
  }

  cat("\nread_source: reading ", length(files),
      " files from ", path, " (pattern: ", pattern, ")\n", sep = "")

  df_list <- purrr::map(files, function(f) {
    raw <- do.call(rio::import, c(list(f), import_args))
    df  <- if (!is.null(pivot_fn)) pivot_fn(raw) else raw
    dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
  })

  combined <- dplyr::bind_rows(df_list)
  cat("read_source: combined dims = ",
      paste(dim(combined), collapse = " x "), "\n")
  combined
}
