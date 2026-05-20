library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

#' Build Full Trap × Year × Week × Species Skeleton
#'
#' Creates the rectangular grid into which all observed data is joined. This
#' function produces the structure only — no cleaning, no status assignment.
#'
#' Boulder County (BC) traps are included regardless of active status because
#' their pool submissions are inconsistent; excluding inactive BC traps drops
#' legitimate surveillance records.
#'
#' zone2 and method are fixed attributes per trap (not expanded):
#'   zone2  = "FC" for NE/NW/SE/SW, else zone.
#'   method = "G" if trap_id contains "gr", else "L".
#'
#' Per-trap start years come from the trap_ref `start` column so that traps
#' deployed mid-surveillance do not generate rows before they existed.
#' year_start is a floor that filters out any year below it.
#'
#' @param trap_ref  Data frame from foco_trap - data.csv.
#'   Required columns: trap_id, zone, active (integer 0/1), start (integer year).
#' @param spp_levels  Character vector of species from cfg$spp_levels.
#' @param year_start  Integer floor year (from cfg$year_start).
#' @param year_end    Integer ceiling year (default: current year).
#' @param week_start  Integer first ISO week (from cfg$week_start).
#' @param week_end    Integer last ISO week (from cfg$week_end).
#' @param fc_zones  Character vector of zone codes that collapse to "FC" in zone2.
#'   Defaults to c("NE", "NW", "SE", "SW"). Pass cfg$fc_zone at the call site.
#'
#' @return Data frame: key, trap_id, zone, zone2, method, year, week, spp.
#' @export
expand_trap <- function(
  trap_ref,
  spp_levels,
  year_start = 2006L,
  year_end   = lubridate::year(Sys.Date()),
  week_start = 23L,
  week_end   = 37L,
  fc_zones   = c("NE", "NW", "SE", "SW")
) {
  # Validate required trap_ref columns early to give a clear error message.
  required_cols <- c("trap_id", "zone", "active", "start")
  missing_cols  <- setdiff(required_cols, names(trap_ref))
  if (length(missing_cols) > 0) {
    stop(
      "trap_ref is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Canonical empty schema — returned when no traps qualify.
  empty_skeleton <- dplyr::tibble(
    key    = character(), trap_id = character(),
    zone   = character(), zone2   = character(), method = character(),
    year   = integer(),   week    = integer(),    spp    = character()
  )

  # BC: all traps regardless of active flag.
  # All other zones: active == 1 only.
  trap_keep <- dplyr::bind_rows(
    trap_ref %>% dplyr::filter(zone == "BC"),
    trap_ref %>% dplyr::filter(zone != "BC", active == 1L)
  )

  # Return empty tibble with correct schema if no traps qualify
  if (nrow(trap_keep) == 0L) return(empty_skeleton)

  # Pre-filter traps whose start year is after year_end: seq(start, year_end)
  # would count *downward* in R (seq(2025, 2023) = c(2025,2024,2023)), which
  # is incorrect. Removing these traps early also makes the intent explicit.
  # rowwise_expand() errors on 0-row data frames, so guard with early return.
  trap_keep <- trap_keep %>%
    dplyr::filter(as.integer(start) <= as.integer(year_end))

  if (nrow(trap_keep) == 0L) return(empty_skeleton)

  # Expand: trap × year × week, then cross with spp.
  # rowwise_expand() handles per-trap start years so no trap generates rows
  # before its deployment date.
  # .year_end, .w_start, .w_end are helper columns to feed rowwise_expand().
  skeleton <- trap_keep %>%
    dplyr::mutate(
      .year_end = as.integer(year_end),
      .w_start  = as.integer(week_start),
      .w_end    = as.integer(week_end)
    ) %>%
    rowwise_expand("start", ".year_end", "year") %>%
    rowwise_expand(".w_start", ".w_end",  "week") %>%
    # Apply year_start floor: drop rows from before the surveillance baseline
    dplyr::filter(year >= as.integer(year_start)) %>%
    dplyr::select(trap_id, zone, year, week) %>%
    # Cross with species vector to create one row per trap×year×week×spp
    tidyr::crossing(spp = spp_levels) %>%
    dplyr::mutate(
      # zone2 collapses all four Fort Collins zones into a single "FC" label
      zone2  = dplyr::if_else(zone %in% fc_zones, "FC", zone),
      # method is determined by trap name: "gr" suffix indicates gravid trap
      method = dplyr::if_else(
        stringr::str_detect(tolower(trap_id), "gr"), "G", "L"
      )
    ) %>%
    dplyr::select(trap_id, zone, zone2, method, year, week, spp)

  # Append composite key: trap_id|spp|year|week (non-alphanumeric stripped)
  make_key(skeleton, key_cols = c("trap_id", "spp", "year", "week"))
}
