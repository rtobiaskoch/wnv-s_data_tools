#' Initialise / Truncate the Pipeline Manifest File
#'
#' Writes a fresh header. Subsequent manifest_log() calls append.
#' Call once at the top of the pipeline so a stale manifest from a prior
#' run never bleeds into the current one.
#'
#' @param path Character. Path to the manifest file.
#' @return Invisibly returns the path.
#' @export
manifest_init <- function(manifest_path) {
  dir.create(dirname(manifest_path), showWarnings = FALSE, recursive = TRUE)
  lines <- c(
    "============================================================",
    "  WNV-S TRAP PIPELINE MANIFEST",
    paste0("  Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("  R version: ", R.version.string),
    "============================================================",
    ""
  )
  writeLines(lines, manifest_path)
  invisible(manifest_path)
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
manifest_log <- function(manifest_path, label, ...) {
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
  cat(paste(lines, collapse = "\n"), file = manifest_path, append = TRUE)
  cat(paste(lines, collapse = "\n"), "\n")
  invisible(manifest_path)
}

#' Count Source Files Matching a Pattern
#'
#' Recursively lists files under `path` matching `pattern` and returns
#' the count and file names. Replaces the old week-folder inventory which
#' assumed a nested WNV-s YYYY/Week WW structure.
#'
#' @param path    Character. Root directory to search.
#' @param pattern Character. Regex passed to list.files().
#' @return Named list: n_files (integer), files (character vector of paths).
#' @export
inventory_source <- function(path, pattern) {
  files <- list.files(path, pattern = pattern,
                      recursive = TRUE, ignore.case = TRUE, full.names = FALSE)
  list(
    n_files = length(files),
    files   = files
  )
}
