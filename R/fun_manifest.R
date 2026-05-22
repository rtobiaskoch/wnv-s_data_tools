#' Initialise / Truncate the Pipeline Manifest File
#'
#' Writes a fresh header. Subsequent manifest_log() calls append.
#' Call once at the top of the pipeline so a stale manifest from a prior
#' run never bleeds into the current one.
#'
#' @param path Character. Path to the manifest file.
#' @return Invisibly returns the path.
#' @export
manifest_init <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  lines <- c(
    "============================================================",
    "  WNV-S TRAP PIPELINE MANIFEST",
    paste0("  Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("  R version: ", R.version.string),
    "============================================================",
    ""
  )
  writeLines(lines, path)
  invisible(path)
}

#' Append a Labeled Entry to the Manifest
#'
#' Writes one block per call. Named arguments become indented lines under
#' the label. Use this for row-count and diagnostic checkpoints.
#'
#' @param path  Character. Manifest file path.
#' @param label Character. Section heading for this entry.
#' @param ...   Named arguments. Each becomes one indented line.
#' @export
manifest_log <- function(path, label, ...) {
  args <- list(...)
  body <- if (length(args) == 0L) {
    character()
  } else {
    nms <- names(args)
    purrr::map2_chr(nms, args, function(n, v) {
      v_chr <- if (length(v) > 1L) paste(v, collapse = ", ") else as.character(v)
      sprintf("  %-30s %s", paste0(n, ":"), v_chr)
    })
  }
  lines <- c(paste0("[", label, "]"), body, "")
  cat(paste(lines, collapse = "\n"), file = path, append = TRUE)
  cat(paste(lines, collapse = "\n"), "\n")
  invisible(path)
}

#' Inventory Week-Level Folders Under WNV-s Year Folders
#'
#' Walks `root`, finds each `WNV-s YYYY*` year folder, then finds each
#' `Week WW` or `wWW` subfolder. Reports how many of those week folders
#' contain at least one file matching `pattern` (recursively).
#'
#' @param root    Character. Root directory holding `WNV-s YYYY*` folders.
#' @param pattern Character. Regex for the target trap file.
#' @return Named list: n_year_folders, n_week_folders, n_matched,
#'   missing_folders (character vector of week folders without a match).
#' @export
inventory_week_folders <- function(root, pattern) {
  year_folders <- list.dirs(root, recursive = FALSE)
  year_folders <- year_folders[grepl("WNV-s 20", basename(year_folders))]

  week_folders <- unlist(lapply(year_folders, function(yf) {
    sub <- list.dirs(yf, recursive = FALSE)
    sub[grepl("^(Week\\s*\\d+|w\\d+)$", basename(sub), ignore.case = TRUE)]
  }))

  matched <- vapply(week_folders, function(wf) {
    length(list.files(wf, pattern = pattern,
                      recursive = TRUE, ignore.case = TRUE)) > 0L
  }, logical(1))

  list(
    n_year_folders  = length(year_folders),
    n_week_folders  = length(week_folders),
    n_matched       = sum(matched),
    missing_folders = week_folders[!matched]
  )
}
