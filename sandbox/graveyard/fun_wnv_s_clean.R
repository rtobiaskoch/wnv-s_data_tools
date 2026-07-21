#' Clean Summary
#'
#' Report the outcome of a per-column cleaning step in a single semantic line.
#' Each call classifies the column's transition from `df0` -> `df` into one of:
#'   - added              (column did not exist in input)
#'   - no-op              (column existed, no values changed, no new NAs)
#'   - transformed        (values changed, no new NAs introduced)
#'   - cleaned (warning)  (values changed AND new NAs introduced)
#'
#' @param df0 The input data frame, before the cleaning step.
#' @param df  The output data frame, after the cleaning step.
#' @param col_name Unquoted column name to summarise.
#' @param label Optional display label; defaults to the deparsed `col_name`.
#'
#' @return Invisibly NULL. Called for the side effect of printing a cli alert.
#'
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo as_name
#' @export
clean_summary <- function(
  df0,
  df,
  col_name,
  label = deparse(substitute(col_name))
) {
  col        <- rlang::enquo(col_name)
  col_string <- rlang::as_name(col)
  n_rows     <- nrow(df)

  # Case 1: column did not exist in input — it was added by the cleaning step.
  if (!col_string %in% names(df0)) {
    cli::cli_alert_info("{.field {label}} added ({n_rows} rows)")
    return(invisible(NULL))
  }

  # Coerce to character so type changes (e.g. character -> Date) compare
  # correctly without triggering charToDate. Comparison uses na.rm = TRUE
  # so NA-vs-NA rows contribute to neither changed nor unchanged.
  old_vals <- as.character(dplyr::pull(df0, !!col))
  new_vals <- as.character(dplyr::pull(df,  !!col))

  changed       <- sum(old_vals != new_vals, na.rm = TRUE)
  na_in         <- sum(is.na(old_vals))
  na_out        <- sum(is.na(new_vals))
  na_introduced <- max(0L, na_out - na_in)

  # Case 2: nothing happened to this column — quiet bullet.
  if (changed == 0 && na_introduced == 0) {
    cli::cli_alert("{.field {label}} no-op")
    return(invisible(NULL))
  }

  # Case 3: values changed AND new NAs appeared — surface as warning so the
  # user notices that some rows were nullified (e.g. failed regex, bad parse).
  if (na_introduced > 0) {
    cli::cli_alert_warning(
      "{.field {label}} cleaned ({changed} changed, {na_introduced} new NA)"
    )
    return(invisible(NULL))
  }

  # Case 4: clean transformation — values changed, no NA introduced.
  cli::cli_alert_success(
    "{.field {label}} transformed ({changed}/{n_rows} rows changed)"
  )
  invisible(NULL)
}

#' Clean a Culex Surveillance Data Sheet
#'
#' Processes and standardizes raw Culex mosquito surveillance data.
#' Trims whitespace from character columns, parses collection dates using
#' parse_flexible_date(), assigns species and method categories, and
#' returns a cleaned data frame with selected columns.
#'
#' @param df A data frame containing raw Culex surveillance data. Must include the columns:
#' trap_name, date_trap_set, mosquito_species, trap_type, mosquito_count, and zone.
#'
#' @return A cleaned and standardized data frame with columns:
#' trap_id, trap_date, year, week, zone, spp, method, and total.
#'
#' @details This function depends on parse_flexible_date() which must be defined elsewhere
#' in the package or user's environment.
#'
#' @examples
#' \dontrun{
#' clean_df <- culex_sheet_clean(raw_df)
#' }
#'
#' @importFrom dplyr mutate across transmute case_when
#' @importFrom stringr str_detect
#' @importFrom purrr map_chr
#' @importFrom lubridate year week
#' @export

wnv_s_clean <- function(
  df,
  all_cols = c(
    "csu_id",
    "trap_id",
    "zone",
    "zone2",
    "trap_date",
    "year",
    "week",
    "spp",
    "spp0",
    "method",
    "trap_status",
    "total"
  ),
  rm_col = c()
) {
  #save original input for comparison
  df0 = df

  # Check required cleaned columns
  col_2_clean = setdiff(all_cols, rm_col)
  present_cols <- intersect(all_cols, names(df))
  missing_cols <- setdiff(all_cols, names(df))

  if (length(missing_cols) > 0) {
    cat(
      "\n Notice. Following are not present for cleaning: ",
      paste(missing_cols, collapse = ", "),
      "\n"
    )
    cat("Run key_rename to convert columns to standard naming convention.")
  }

  if (length(present_cols) > 0) {
    cat(
      "\n The Following columns are being cleaned: ",
      paste(present_cols, collapse = ", "),
      "\n"
    )
  }

  # Trim whitespace from all character columns
  df <- df %>%
    mutate(across(where(is.character), trimws))

  # CLEAN csu_id
  if ("csu_id" %in% names(df) && "csu_id" %in% col_2_clean) {
    df <- df %>%
      mutate(csu_id = str_remove(csu_id, "-"))

    clean_summary(df0, df, csu_id)
  }

  # CLEAN ZONE
  if ("zone" %in% names(df) && "zone2" %in% col_2_clean) {
    valid_zones <- c("NE", "NW", "SE", "SW", "LV", "BE", "BC")
    zone_pattern <- str_c(valid_zones, collapse = "|")

    # Normalize "Berthoud" label then extract standard zone code in one pass
    df <- df %>%
      mutate(
        zone = if_else(str_detect(zone, "Berthoud"), "BE", zone),
        zone = str_extract(zone, zone_pattern)
      )

    clean_summary(df0, df, zone)
  }

  # CLEAN/GET ZONE2
  if ("zone" %in% names(df) && "zone2" %in% col_2_clean) {
    fc_zones <- c("NE", "NW", "SE", "SW")

    df <- df %>%
      mutate(zone2 = if_else(zone %in% fc_zones, "FC", zone))

    clean_summary(df0, df, zone2)
  }

  # FALLBACK: derive zone2 (and zone) from trap_id prefix when the zone
  # column is absent from the input — e.g. when an upstream rename step has
  # dropped it. Trap-id prefixes map deterministically to zone2:
  #   FC-*  -> FC   (Fort Collins; underlying NE/NW/SE/SW lost without zone)
  #   LV-*  -> LV   (Loveland)
  #   BE-*  -> BE   (Berthoud current naming)
  #   LC-*  -> BE   (Berthoud legacy CDC traps renamed from BE-***)
  #   WC-*  -> BE   (Berthoud West)
  #   BC-*  -> BC   (Boulder County)
  # zone is set equal to zone2 here so downstream code that expects both
  # columns (e.g. prep_for_skeleton's distinct()) works without errors.
  # Specifying zone exactly (NE/NW/SE/SW) is impossible from trap_id alone
  # for Fort Collins — those rows carry zone = "FC" as the best available value.
  if (
    !"zone2" %in% names(df) &&
      "zone2" %in% col_2_clean &&
      "trap_id" %in% names(df)
  ) {
    df <- df %>%
      mutate(
        zone2 = case_when(
          str_detect(trap_id, "^(?i)FC") ~ "FC",
          str_detect(trap_id, "^(?i)LV") ~ "LV",
          str_detect(trap_id, "^(?i)(BE|LC|WC)") ~ "BE",
          str_detect(trap_id, "^(?i)BC") ~ "BC",
          TRUE ~ NA_character_
        )
      )
    if (!"zone" %in% names(df)) {
      df <- df %>% mutate(zone = zone2)
    }

    clean_summary(df0, df, zone2)
  }

  #CLEAN DATE
  if ("trap_date" %in% names(df) && "trap_date" %in% col_2_clean) {
    df <- df %>%
      mutate(
        trap_date = purrr::map_chr(
          trap_date,
          ~ as.character(parse_flexible_date(.x))
        ),
        trap_date = as.Date(trap_date)
      )

    clean_summary(df0, df, trap_date)
  }

  # GET YEAR AND WEEK
  # Handles two cases: columns absent (derive entirely from trap_date) or
  # present as character (fill NAs, coercing to integer to match lubridate output)
  if ("trap_date" %in% names(df) && "year" %in% col_2_clean) {
    has_year <- "year" %in% names(df)
    has_week <- "week" %in% names(df)

    # Capture the submitter-entered week (if present) before we overwrite it, so
    # disagreements with the date-derived week can be surfaced as a QC signal.
    submitter_week <- if (has_week) as.integer(df$week) else NA_integer_

    df <- df %>%
      mutate(
        year = if (has_year) {
          dplyr::coalesce(as.integer(year), lubridate::year(trap_date))
        } else {
          lubridate::year(trap_date)
        },
        # Seasonal/reported week from trap_date is the SINGLE week authority
        # (wnvSurv::calc_season_week): the first full week of June is always week
        # 23, leap-week-stable. This is the IDENTICAL rule the weekly report
        # applies to incoming pools and counts, so a pool and the trap count it
        # came from always land in the same week. The submitter-typed week is no
        # longer trusted for the value (only used for the QC check below).
        week = wnvSurv::calc_season_week(trap_date),
        # Bookkeeping: keep the original human-entered week (NA if none) so the
        # corrected `week` stays auditable in the final database.
        week_submitted = submitter_week
      )

    # QC: flag rows where the submitter-typed week disagrees with the
    # date-derived week by more than 1 (usually a bad Trap Date or mis-keyed week).
    if (has_week) {
      n_wk_mismatch <- sum(abs(submitter_week - df$week) > 1, na.rm = TRUE)
      if (n_wk_mismatch > 0) {
        cli::cli_alert_warning(
          "{n_wk_mismatch} row{?s}: submitter week differs from trap_date-derived week by >1 (check Trap Date)"
        )
      }
    }

    clean_summary(df0, df, year)
    clean_summary(df0, df, week)
  } #end if trap_date

  # CLEAN TRAP_ID — strip all whitespace and uppercase. Source spreadsheets
  # have introduced case and internal-whitespace variation (e.g. "lc-001",
  # "LC- 001") that must be canonicalised before joins and method derivation.
  if ("trap_id" %in% names(df) && "trap_id" %in% col_2_clean) {
    df <- df %>%
      mutate(trap_id = toupper(stringr::str_remove_all(trap_id, "\\s+")))

    clean_summary(df0, df, trap_id)
  }

  #GET METHOD
  if ("trap_id" %in% names(df) && "trap_id" %in% col_2_clean) {
    df <- df %>%
      mutate(
        method = case_when(
          str_detect(tolower(trap_id), "gr") ~ "G",
          TRUE ~ "L"
        )
      )

    clean_summary(df0, df, method)
  }

  # SAVE SPP0 — always snapshot the raw spp before any cleaning whenever spp
  # is present. spp0 is required internally by the CREATE TRAP_STATUS block
  # regardless of col_2_clean / rm_col settings.
  if ("spp" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(spp0 = spp)

    clean_summary(df0, df, spp0)
  }

  # CREATE TRAP_STATUS — derived from spp0 (raw value saved above).
  # Grouped by trap_id + trap_date so a single malfunction or culex record
  # sets the status for all rows from that trap-night.
  if ("spp" %in% names(df) && "trap_status" %in% col_2_clean) {
    if (!"trap_status" %in% names(df)) {
      df$trap_status <- NA_character_
    }

    df <- df %>%
      dplyr::group_by(trap_id, trap_date) %>%
      dplyr::mutate(
        trap_status = dplyr::case_when(
          # Standardise legacy uppercase "No Traps" set by expand_trap_spp()
          any(
            trap_status %in% c("No Traps", "no trap"),
            na.rm = TRUE
          ) ~ "no trap",
          any(trap_status == "malfunction", na.rm = TRUE) ~ "malfunction",
          any(
            stringr::str_detect(
              spp0,
              "(?i)malfunction|stolen|vandalized"
            ),
            na.rm = TRUE
          ) ~ "malfunction",
          any(
            stringr::str_detect(
              spp0,
              "(?i)no mosquitoes"
            ),
            na.rm = TRUE
          ) ~ "no mosquitoes",
          # total > 0 guard: 0-fill rows added by spp expansion must not
          # trigger "culex" for trap-weeks where nothing was actually caught
          any(
            stringr::str_detect(spp0, "(?i)tarsalis|pipiens") & total > 0,
            na.rm = TRUE
          ) ~ "culex",
          any(
            stringr::str_detect(spp0, "(?i)tarsalis|pipiens"),
            na.rm = TRUE
          ) ~ "no mosquitoes",
          TRUE ~ "no culex"
        )
      ) %>%
      dplyr::ungroup()

    # Set total = NA for malfunction BEFORE the CLEAN TOTAL block runs below.
    # This is the operative coercion — CLEAN TOTAL becomes a no-op for these rows.
    # Malfunction traps did not collect data — total must be NA so they are
    # not counted as zeros in abundance calculations.
    df <- df %>%
      dplyr::mutate(
        total = dplyr::if_else(
          trap_status == "malfunction",
          NA_real_,
          as.numeric(total)
        )
      )

    clean_summary(df0, df, trap_status)
  }

  # CLEAN SPP — standardise to Tarsalis / Pipiens / none / non culex.
  # spp0 was already saved in the block above.
  if ("spp" %in% names(df) && "spp" %in% col_2_clean) {
    df <- df %>%
      dplyr::mutate(
        spp = dplyr::case_when(
          stringr::str_detect(spp, "(?i)Tarsalis") ~ "Tarsalis",
          stringr::str_detect(spp, "(?i)Pipiens") ~ "Pipiens",
          stringr::str_detect(
            spp,
            "(?i)malfunction|stolen|no mosquitoes"
          ) ~ "none",
          TRUE ~ "non culex"
        )
      )
    clean_summary(df0, df, spp)
  }

  # Convert total count to numeric
  if ("total" %in% names(df) && "total" %in% col_2_clean) {
    df <- df %>%
      mutate(total = as.numeric(total))

    clean_summary(df0, df, total)
  }

  if ("trap_date" %in% names(df) && "trap_id" %in% names(df)) {
    df <- df %>%
      select(any_of(all_cols), everything()) %>%
      arrange(desc(trap_date), trap_id)
  } else {
    df <- df %>%
      select(any_of(all_cols), everything())
  }

  return(df)
}
