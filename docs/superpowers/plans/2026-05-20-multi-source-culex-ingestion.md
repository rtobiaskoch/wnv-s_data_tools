# Multi-Source Culex Ingestion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the consolidated `1_input/all_mosq/LC * all mosquitoes.csv` inputs with the original per-source individual files (CFC 2006–2017, CMC weekly 2015–2018, VDCI weekly 2019–present, Boulder) so the trap-level dataset is built end-to-end from raw sources, with `wnv-s_database` still patching gaps.

**Architecture:** A single polymorphic loader `read_source()` walks each source folder using a config-declared `path + pattern`, dispatches to a per-source `pivot_fn` (only when the raw format needs reshaping), then funnels every source through the existing canonical chain (`key_rename → wnv_s_clean → make_key → culex_dedup → prep_for_skeleton`). Each prepped source is added to the `fill_skeleton()` priority list (VDCI → CMC → CFC → BC → database). The existing `database` supplement, post-expand status logic, and all five current `stopifnot()` validations remain untouched.

**Tech Stack:** R 4.x, tidyverse (dplyr, tidyr, purrr, stringr, lubridate), `rio` for heterogeneous file I/O, `config` for YAML, `testthat` for unit tests, Quarto for the pipeline document.

---

## File Structure

**New R files (one responsibility each):**
| File | Responsibility |
|---|---|
| `R/fun_read_source.R` | Dispatcher: list files matching `path + pattern`, read with `rio::import`, optionally apply per-source `pivot_fn`, bind. |
| `R/fun_pivot_cfc.R` | CFC 2006–2017 wide→long pivot (`Tarsalis`/`Pipiens` columns → `mosquito_species` rows). |
| `R/fun_pivot_cmc.R` | CMC weekly 2015–2018 wide→long pivot (`Cx tarsalis`/`Cx pipiens` columns → `mosquito_species` rows; preserves `Malfunction` flag). |
| `R/fun_pivot_boulder.R` | Boulder reshape — many Boulder sheets carry a non-canonical column layout; this returns a frame keyed off `trap_name`, `mosquito_species`, `mosquito_count`, `date_trap_set`, `zone`. |

**Modified files:**
| File | What changes |
|---|---|
| `1_input/database_column_rename.csv` | Add aliases for CMC/CFC/Boulder columns (`Cx tarsalis`, `Cx pipiens`, `Trap Number`, `Trap Date`, `Light / Gravid`, etc.) so `key_rename()` produces canonical columns. |
| `config/config_culex_combine.yml` | Add `sources:` block declaring `path`, `pattern`, optional `pivot_fn`, `filter_active`, and priority order. Remove the obsolete top-level `pattern` once `read_list()` is no longer called. |
| `pipelines/pipeline_combine_culex_sheet.qmd` | Replace the single `read_list()` import chunk with a per-source loop. Each source emits a prepped frame; the named list is passed to `fill_skeleton()`. New completeness `stopifnot()` after the join. |

**Removed dependencies:** `1_input/all_mosq/` is no longer read by the pipeline. The folder stays on disk for validation comparisons.

---

## Task 1: Inventory the source files

**Goal:** Confirm the file-name patterns match every expected file before writing code against them. Patterns that miss files silently produce wrong totals.

**Files:**
- Discovery only — no edits.

- [ ] **Step 1: List every candidate file under each source root**

Run:
```bash
echo "=== CFC ==="
find "1_input/culex_sheet/CFC-2006-2017" -type f -name "*.xls*"

echo "=== CMC weekly (2015-2018) ==="
find "1_input/culex_sheet/WNV-s 2015 (RAM)" "1_input/culex_sheet/WNV-s 2016 (RAM)" \
     "1_input/culex_sheet/WNV-s 2017 (MCY)" "1_input/culex_sheet/WNV-s 2018 (MCY)" \
     -type f -iname "LC Week*Full*.xls*"

echo "=== VDCI weekly (2019+) ==="
find "1_input/culex_sheet" -type f \( -iname "LC Week*Culex.csv" -o -iname "LC Week*culex.xlsx" -o -iname "LC Week*_Culex.xls" \) \
     -not -path "*all_mosq*" \
     -not -path "*CFC-2006-2017*"

echo "=== Boulder ==="
find "1_input/culex_sheet" -type f \( -iname "Boulder*Culex.xlsx" -o -iname "Abundance_Culex*.csv" \) \
     -not -path "*all_mosq*"
```
Expected: At least 1 CFC file, ~60 CMC weekly files (2015–2018 weeks 23–37), ~80 VDCI weekly files (2019–2025 weeks 23–37), ~30 Boulder files (2021–2025).

- [ ] **Step 2: Spot-check column headers per source**

Open one file from each source in R:
```r
rio::import("1_input/culex_sheet/CFC-2006-2017/CFC_Full_Raw_Data_2006-2017.xlsx") |> names()
rio::import("1_input/culex_sheet/WNV-s 2018 (MCY)/Week 30/Data from CMC/LC Week30_2018_Full_009.xls") |> names()
rio::import("1_input/culex_sheet/WNV-s 2020 (MCY)/Week 25/Data from VDCI/LC Week25_2020_Culex.csv") |> names()
rio::import("1_input/culex_sheet/WNV-s 2022 (LNW)/Week 24/Data from VDCI and TA/Boulder 13June_2022_Culex.xlsx") |> names()
```
Record any column names not already in `1_input/database_column_rename.csv` — these become rows added in Task 2.

- [ ] **Step 3: Commit the inventory as a note**

Create `docs/source_file_inventory.md` containing the four file lists from Step 1 and the column-name observations from Step 2. This is the source of truth for patterns used in Task 3.

```bash
git add docs/source_file_inventory.md
git commit -m "docs: inventory source files for multi-source ingestion"
```

---

## Task 2: Extend `database_column_rename.csv` with source-specific aliases

**Goal:** `key_rename()` is the single column-translation hop. All source-specific column names must resolve to canonical names here so the per-source clean functions don't each invent their own rename logic.

**Files:**
- Modify: `1_input/database_column_rename.csv`

- [ ] **Step 1: Add aliases**

Open `1_input/database_column_rename.csv` and append the rows below (existing rows stay untouched). Use canonical names from CLAUDE.md's data dictionary: `trap_id`, `trap_date`, `spp`, `method`, `total`, `zone`, `week`, `year`, `trap_status`.

```csv
trap_id,Trap Number
trap_date,Trap Date
zone,Zone
method,Light / Gravid
total,Total CX
total,Total Females
trap_status,Malfunction
spp,Cx tarsalis
spp,Cx pipiens
```

Note: `Cx tarsalis` / `Cx pipiens` map to `spp` because after the CMC pivot (Task 5) those become row-level species labels in the long format. The `Total CX` / `Total Females` mappings let `wnv_s_clean()` produce a numeric `total` if the source has no per-species count column.

- [ ] **Step 2: Verify the file parses**

Run:
```r
readr::read_csv("1_input/database_column_rename.csv", show_col_types = FALSE)
```
Expected: No parse errors, ~35 rows, two columns `new` and `old`.

- [ ] **Step 3: Commit**

```bash
git add 1_input/database_column_rename.csv
git commit -m "feat: add CMC/CFC/Boulder column aliases to rename key"
```

---

## Task 3: Add a `sources:` block to the config

**Goal:** Declarative source registry — adding a future source (e.g., CMC pre-2015) becomes one YAML entry, not a code change.

**Files:**
- Modify: `config/config_culex_combine.yml`

- [ ] **Step 1: Add the block under `default:`**

Insert after the `path:` line:

```yaml
  sources:
    vdci:
      path: "1_input/culex_sheet"
      pattern: "LC Week.*_.*[Cc]ulex\\.(csv|xlsx)$"
      pivot_fn: null
      filter_active: true
      exclude_paths:
        - "all_mosq"
        - "CFC-2006-2017"
    cmc_weekly:
      path: "1_input/culex_sheet"
      pattern: "LC Week.*Full.*\\.xls[x]?$"
      pivot_fn: "pivot_cmc"
      filter_active: true
      exclude_paths:
        - "all_mosq"
    cfc:
      path: "1_input/culex_sheet/CFC-2006-2017"
      pattern: "CFC_Full_Raw_Data.*\\.xlsx$"
      pivot_fn: "pivot_cfc"
      filter_active: true
    bc:
      path: "1_input/culex_sheet"
      pattern: "(Boulder .*_.*[Cc]ulex|Abundance_Culex_.*)\\.(csv|xlsx)$"
      pivot_fn: "pivot_boulder"
      filter_active: false
      exclude_paths:
        - "all_mosq"
```

`pivot_fn: null` for VDCI means "no reshape needed — already long". `filter_active: false` for Boulder skips the `trap_keep_df` filter (Boulder keeps all traps per CLAUDE.md).

- [ ] **Step 2: Verify config parses**

Run:
```r
cfg <- config::get(file = "config/config_culex_combine.yml", config = "default")
str(cfg$sources)
```
Expected: A named list of length 4 (`vdci`, `cmc_weekly`, `cfc`, `bc`), each a list with `path`, `pattern`, `pivot_fn`, `filter_active`.

- [ ] **Step 3: Commit**

```bash
git add config/config_culex_combine.yml
git commit -m "feat: declare per-source ingestion config block"
```

---

## Task 4: Build the `read_source()` dispatcher

**Goal:** One function that takes a single source-config entry and returns a bound data frame of all raw rows from that source, with the optional `pivot_fn` applied per file. Replaces `read_list()`.

**Files:**
- Create: `R/fun_read_source.R`
- Test: `tests/testthat/test-read_source.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-read_source.R
test_that("read_source returns empty tibble when no files match", {
  result <- read_source(
    path = tempdir(),
    pattern = "nothing_matches\\.csv$",
    pivot_fn = NULL,
    exclude_paths = character()
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("read_source excludes paths containing exclude tokens", {
  tmp <- tempfile(); dir.create(file.path(tmp, "all_mosq"), recursive = TRUE)
  writeLines("a,b\n1,2", file.path(tmp, "all_mosq", "x.csv"))
  writeLines("a,b\n3,4", file.path(tmp, "x.csv"))
  result <- read_source(
    path = tmp, pattern = "\\.csv$", pivot_fn = NULL,
    exclude_paths = "all_mosq"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(as.character(result$a), "3")
})

test_that("read_source applies pivot_fn when provided", {
  tmp <- tempfile(); dir.create(tmp, recursive = TRUE)
  writeLines("trap,tar,pip\nFC-001,3,5", file.path(tmp, "wide.csv"))
  pivot <- function(df) {
    tidyr::pivot_longer(df, c(tar, pip),
                        names_to = "mosquito_species",
                        values_to = "mosquito_count")
  }
  result <- read_source(
    path = tmp, pattern = "\\.csv$", pivot_fn = pivot,
    exclude_paths = character()
  )
  expect_equal(nrow(result), 2L)
  expect_setequal(result$mosquito_species, c("tar", "pip"))
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-read_source.R")'`
Expected: 3 errors, "could not find function 'read_source'".

- [ ] **Step 3: Implement `read_source()`**

```r
# R/fun_read_source.R
#' Read All Files Under a Source Root
#'
#' Lists files recursively under `path` matching `pattern`, optionally drops
#' paths containing any string in `exclude_paths`, reads each with rio::import,
#' applies an optional per-file pivot function, then row-binds.
#'
#' Replaces read_list() in the multi-source pipeline. Each invocation handles
#' exactly ONE source (one folder root, one pattern, one pivot rule).
#'
#' @param path Character. Directory to search recursively.
#' @param pattern Character. Regex passed to list.files().
#' @param pivot_fn Function or NULL. If non-NULL, applied to each file's
#'   imported data frame BEFORE binding. Use to reshape wide formats to long.
#' @param exclude_paths Character vector. File paths containing any of these
#'   substrings are dropped (e.g., "all_mosq" to skip the consolidated files).
#'
#' @return A tibble of all rows from all matched files, columns coerced to
#'   character (matches read_list() behaviour for downstream compatibility).
#' @export
read_source <- function(path, pattern, pivot_fn = NULL,
                        exclude_paths = character()) {
  files <- list.files(
    path = path, pattern = pattern,
    recursive = TRUE, full.names = TRUE, ignore.case = TRUE
  )

  if (length(exclude_paths) > 0L) {
    drop_re <- paste(exclude_paths, collapse = "|")
    files <- files[!stringr::str_detect(files, drop_re)]
  }

  if (length(files) == 0L) {
    warning("read_source: no files matched ", pattern, " under ", path)
    return(tibble::tibble())
  }

  cat("\nread_source: reading ", length(files),
      " files from ", path, " (pattern: ", pattern, ")\n", sep = "")

  df_list <- purrr::map(files, function(f) {
    raw <- rio::import(f)
    df  <- if (!is.null(pivot_fn)) pivot_fn(raw) else raw
    dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
  })

  combined <- dplyr::bind_rows(df_list)
  cat("read_source: combined dims = ", paste(dim(combined), collapse = " x "), "\n")
  combined
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-read_source.R")'`
Expected: 3 passes.

- [ ] **Step 5: Commit**

```bash
git add R/fun_read_source.R tests/testthat/test-read_source.R
git commit -m "feat: add read_source() dispatcher for per-source ingestion"
```

---

## Task 5: Build `pivot_cmc()` for CMC weekly 2015–2018 sheets

**Goal:** CMC weekly sheets are wide (one column per species). `wnv_s_clean()` expects one row per (trap, species). Pivot wide→long here, preserving the `Malfunction` flag by emitting a species placeholder string that `wnv_s_clean()` already recognizes.

**Files:**
- Create: `R/fun_pivot_cmc.R`
- Test: `tests/testthat/test-pivot_cmc.R`

- [ ] **Step 1: Write the failing test**

```r
# tests/testthat/test-pivot_cmc.R
test_that("pivot_cmc emits one row per (trap, species)", {
  raw <- tibble::tibble(
    `Trap Number`  = c("FC-029gr", "LV-020"),
    `Trap Date`    = c("06/16/2016", "06/15/2016"),
    `Zone`         = c("SE", "LV"),
    `Light / Gravid` = c("GRAVID", "LIGHT"),
    `Malfunction`  = c("NO", "NO"),
    `Cx tarsalis`  = c("0", "17"),
    `Cx pipiens`   = c("25", "0"),
    `Total CX`     = c("25", "17"),
    `Week`         = c("24", "24")
  )
  out <- pivot_cmc(raw)
  expect_equal(nrow(out), 4L)
  expect_setequal(out$mosquito_species, c("Cx tarsalis", "Cx pipiens"))
  expect_true(all(c("trap_name", "mosquito_count", "date_trap_set", "Zone") %in% names(out)))
})

test_that("pivot_cmc emits 'malfunction' row when Malfunction != NO", {
  raw <- tibble::tibble(
    `Trap Number` = "FC-001",
    `Trap Date`   = "06/16/2016",
    `Zone`        = "SE",
    `Light / Gravid` = "LIGHT",
    `Malfunction` = "YES",
    `Cx tarsalis` = NA, `Cx pipiens` = NA, `Total CX` = NA,
    `Week` = "24"
  )
  out <- pivot_cmc(raw)
  expect_true(any(stringr::str_detect(out$mosquito_species, "(?i)malfunction")))
})
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-pivot_cmc.R")'`
Expected: 2 errors, "could not find function 'pivot_cmc'".

- [ ] **Step 3: Implement `pivot_cmc()`**

```r
# R/fun_pivot_cmc.R
#' Pivot CMC Weekly Sheets to Long Format
#'
#' CMC 2015-2018 weekly files are wide: one column per species
#' (`Cx tarsalis`, `Cx pipiens`). Downstream cleaning (wnv_s_clean) expects
#' one row per (trap, species) with `mosquito_species` and `mosquito_count`
#' columns. This pivot reshapes the wide sheet and emits a synthetic
#' "malfunction" species row when `Malfunction != "NO"` so wnv_s_clean()'s
#' trap_status logic fires.
#'
#' Source files use highly inconsistent column casing; the function performs
#' no renaming — that is key_rename()'s job downstream. We only reshape.
#'
#' @param df Raw imported CMC weekly sheet.
#' @return A long-format tibble with columns including trap_name,
#'   date_trap_set, Zone, Light / Gravid, mosquito_species, mosquito_count.
#' @export
pivot_cmc <- function(df) {
  # Standardise the two common column-name variants seen across years.
  # We do this here (not in key_rename) because the source filename does
  # not let key_rename know which variant to expect.
  names(df) <- stringr::str_replace_all(names(df), "\\s+", " ")

  spp_cols <- intersect(c("Cx tarsalis", "Cx pipiens", "Cx Tarsalis", "Cx Pipiens"),
                        names(df))
  if (length(spp_cols) == 0L) {
    stop("pivot_cmc: no Cx tarsalis / Cx pipiens columns found. Columns: ",
         paste(names(df), collapse = ", "))
  }

  long <- df %>%
    dplyr::rename(trap_name      = dplyr::any_of("Trap Number"),
                  date_trap_set  = dplyr::any_of("Trap Date")) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(spp_cols),
      names_to  = "mosquito_species",
      values_to = "mosquito_count"
    )

  # Synthesize a malfunction row per trap-date where Malfunction != NO.
  # wnv_s_clean() looks for "malfunction" in spp0 to set trap_status.
  if ("Malfunction" %in% names(long)) {
    malf <- long %>%
      dplyr::filter(!is.na(Malfunction),
                    !stringr::str_detect(Malfunction, "(?i)^no$|^n$")) %>%
      dplyr::distinct(trap_name, date_trap_set, .keep_all = TRUE) %>%
      dplyr::mutate(mosquito_species = "malfunction",
                    mosquito_count   = NA_character_)
    long <- dplyr::bind_rows(long, malf)
  }

  long
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pivot_cmc.R")'`
Expected: 2 passes.

- [ ] **Step 5: Commit**

```bash
git add R/fun_pivot_cmc.R tests/testthat/test-pivot_cmc.R
git commit -m "feat: add pivot_cmc() to reshape CMC weekly sheets long"
```

---

## Task 6: Build `pivot_cfc()` for the CFC 2006–2017 consolidated file

**Goal:** The CFC consolidated file covers 12 years in one xlsx. Inspect headers, pivot to the same long shape `pivot_cmc()` produces. The downstream chain treats CFC rows identically to CMC rows.

**Files:**
- Create: `R/fun_pivot_cfc.R`
- Test: `tests/testthat/test-pivot_cfc.R`

- [ ] **Step 1: Inspect the CFC file headers**

Run interactively:
```r
cfc <- rio::import("1_input/culex_sheet/CFC-2006-2017/CFC_Full_Raw_Data_2006-2017.xlsx")
names(cfc)
head(cfc, 3)
```
Use the actual column names observed to fill in the test fixture below. **Do not assume** — the exact names matter. (Common observed: `Trap`, `Date`, `Tarsalis`, `Pipiens`, `Other`, `Zone`, `Type`. If your file differs, adjust the test to match.)

- [ ] **Step 2: Write the failing test using the observed schema**

```r
# tests/testthat/test-pivot_cfc.R
test_that("pivot_cfc emits one row per (trap, species)", {
  raw <- tibble::tibble(
    Trap     = c("FC-029gr", "LV-020"),
    Date     = c("2010-06-16", "2010-06-15"),
    Zone     = c("SE", "LV"),
    Type     = c("G", "L"),
    Tarsalis = c("0", "17"),
    Pipiens  = c("25", "0")
  )
  out <- pivot_cfc(raw)
  expect_equal(nrow(out), 4L)
  expect_setequal(out$mosquito_species, c("Tarsalis", "Pipiens"))
  expect_true(all(c("trap_name", "date_trap_set") %in% names(out)))
})
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-pivot_cfc.R")'`
Expected: error "could not find function 'pivot_cfc'".

- [ ] **Step 4: Implement `pivot_cfc()`**

```r
# R/fun_pivot_cfc.R
#' Pivot CFC 2006-2017 Consolidated Sheet to Long Format
#'
#' The CFC consolidated file is wide with one column per species. This
#' function reshapes it to long format identical to pivot_cmc() output so
#' the downstream chain (key_rename -> wnv_s_clean -> prep_for_skeleton)
#' treats CFC rows uniformly.
#'
#' Adjust the candidate species column names below if the actual file
#' uses different headers; columns not present are silently skipped.
#'
#' @param df Raw imported CFC consolidated file.
#' @return Long-format tibble with mosquito_species and mosquito_count.
#' @export
pivot_cfc <- function(df) {
  names(df) <- stringr::str_replace_all(names(df), "\\s+", " ")

  spp_candidates <- c("Tarsalis", "Pipiens", "Cx tarsalis", "Cx pipiens")
  spp_cols <- intersect(spp_candidates, names(df))
  if (length(spp_cols) == 0L) {
    stop("pivot_cfc: no species columns found. Columns: ",
         paste(names(df), collapse = ", "))
  }

  df %>%
    dplyr::rename(trap_name     = dplyr::any_of(c("Trap", "Trap Number", "Trap ID")),
                  date_trap_set = dplyr::any_of(c("Date", "Trap Date"))) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(spp_cols),
      names_to  = "mosquito_species",
      values_to = "mosquito_count"
    )
}
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pivot_cfc.R")'`
Expected: 1 pass.

- [ ] **Step 6: Commit**

```bash
git add R/fun_pivot_cfc.R tests/testthat/test-pivot_cfc.R
git commit -m "feat: add pivot_cfc() for consolidated 2006-2017 CFC file"
```

---

## Task 7: Build `pivot_boulder()` for Boulder Culex sheets

**Goal:** Boulder sheets share the VDCI long-format pattern in some years and a wide format in others. Normalize to long, with `mosquito_species` and `mosquito_count` present.

**Files:**
- Create: `R/fun_pivot_boulder.R`
- Test: `tests/testthat/test-pivot_boulder.R`

- [ ] **Step 1: Inspect representative Boulder files**

Run:
```r
b1 <- rio::import("1_input/culex_sheet/WNV-s 2022 (LNW)/Week 24/Data from VDCI and TA/Boulder 13June_2022_Culex.xlsx")
names(b1); head(b1, 3)

b2 <- rio::import("1_input/culex_sheet/WNV-s 2024 (RTK)/w27/Boulder 01July_2024_Culex.xlsx")
names(b2); head(b2, 3)
```
Record both schemas — if both are already long with VDCI-compatible names (`mosquito_species`, `mosquito_count`, `trap_name`, `date_trap_set`), this function becomes a pass-through that only forces a `Zone = "BC"` column when absent.

- [ ] **Step 2: Write the failing test**

```r
# tests/testthat/test-pivot_boulder.R
test_that("pivot_boulder forces zone = BC when zone column is missing", {
  raw <- tibble::tibble(
    trap_name        = "BC-12",
    date_trap_set    = "2022-06-13",
    mosquito_species = "Culex tarsalis",
    mosquito_count   = "8",
    trap_type        = "CDC Light Trap"
  )
  out <- pivot_boulder(raw)
  expect_true("Zone" %in% names(out) || "zone" %in% names(out))
  zcol <- if ("Zone" %in% names(out)) out$Zone else out$zone
  expect_equal(unique(zcol), "BC")
})

test_that("pivot_boulder reshapes wide Boulder sheets to long", {
  # Adjust this fixture to match the wide variant observed in Step 1.
  raw <- tibble::tibble(
    `Trap Number` = "BC-12",
    `Trap Date`   = "2022-06-13",
    `Cx tarsalis` = "8",
    `Cx pipiens`  = "0",
    `Zone`        = "BC"
  )
  out <- pivot_boulder(raw)
  expect_true("mosquito_species" %in% names(out))
  expect_equal(nrow(out), 2L)
})
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-pivot_boulder.R")'`
Expected: errors, "could not find function 'pivot_boulder'".

- [ ] **Step 4: Implement `pivot_boulder()`**

```r
# R/fun_pivot_boulder.R
#' Normalise Boulder Culex Sheets to Long Format with Zone = BC
#'
#' Boulder sheets vary between two schemas across years: a VDCI-style long
#' format and a CMC-style wide format. This function detects which is in
#' use and emits the long form. Zone is forced to "BC" because Boulder
#' sheets sometimes omit the column entirely.
#'
#' @param df Raw imported Boulder sheet.
#' @return Long-format tibble with mosquito_species, mosquito_count, and Zone = BC.
#' @export
pivot_boulder <- function(df) {
  names(df) <- stringr::str_replace_all(names(df), "\\s+", " ")

  wide_spp <- intersect(c("Cx tarsalis", "Cx pipiens", "Cx Tarsalis", "Cx Pipiens"),
                        names(df))

  long <- if (length(wide_spp) > 0L) {
    df %>%
      dplyr::rename(trap_name     = dplyr::any_of(c("Trap Number", "trap_name")),
                    date_trap_set = dplyr::any_of(c("Trap Date", "date_trap_set"))) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(wide_spp),
        names_to  = "mosquito_species",
        values_to = "mosquito_count"
      )
  } else {
    df
  }

  if (!any(c("Zone", "zone") %in% names(long))) {
    long$Zone <- "BC"
  } else {
    zone_col <- if ("Zone" %in% names(long)) "Zone" else "zone"
    long[[zone_col]] <- "BC"
  }
  long
}
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-pivot_boulder.R")'`
Expected: 2 passes.

- [ ] **Step 6: Commit**

```bash
git add R/fun_pivot_boulder.R tests/testthat/test-pivot_boulder.R
git commit -m "feat: add pivot_boulder() to normalise Boulder Culex sheets"
```

---

## Task 8: Rewrite the IMPORT / CLEAN chunks in the QMD to drive per-source

**Goal:** Replace the single `read_list()` import with a loop over `cfg$sources`. Each source goes through the same canonical chain and emits a prepped frame keyed for `fill_skeleton()`.

**Files:**
- Modify: `pipelines/pipeline_combine_culex_sheet.qmd` (replace lines 74–143)

- [ ] **Step 1: Edit the IMPORT and CLEAN chunks**

In `pipelines/pipeline_combine_culex_sheet.qmd`, replace the `#--- B U L K   I M P O R T ---` chunk (lines 74–79) and the `#--- C L E A N ---` chunk (lines 117–143) with the new per-source block below. Keep the SOURCE USED FUNCTIONS block (lines 16–39) and add the new source files there:

In the SOURCE block, add:
```r
source("R/fun_read_source.R")
source("R/fun_pivot_cmc.R")
source("R/fun_pivot_cfc.R")
source("R/fun_pivot_boulder.R")
```

Replace the IMPORT and CLEAN chunks with:

````markdown
#---------------------------  P E R   S O U R C E   I M P O R T  ------------------------------
DESC: read each source declared in cfg$sources, dispatch the optional pivot_fn,
then funnel every source through the canonical clean chain. Each entry of
`sources_prepped` is a fully prepped frame ready for fill_skeleton().

```{r, per_source_import}
# Map config pivot_fn names (strings) to actual functions. NULL = no reshape.
pivot_registry <- list(
  pivot_cmc     = pivot_cmc,
  pivot_cfc     = pivot_cfc,
  pivot_boulder = pivot_boulder
)

sources_raw <- purrr::imap(cfg$sources, function(src, src_name) {
  pivot_fn      <- if (is.null(src$pivot_fn))      NULL          else pivot_registry[[src$pivot_fn]]
  exclude_paths <- if (is.null(src$exclude_paths)) character()   else src$exclude_paths
  read_source(
    path          = src$path,
    pattern       = src$pattern,
    pivot_fn      = pivot_fn,
    exclude_paths = exclude_paths
  )
})

# Diagnostic: row counts per source before cleaning.
purrr::iwalk(sources_raw, ~ cat(sprintf("raw %-12s rows: %d\n", .y, nrow(.x))))
```

#---------------------------  P E R   S O U R C E   C L E A N + P R E P -----------------------
DESC: each source goes through key_rename -> wnv_s_clean -> make_key ->
culex_dedup -> prep_for_skeleton. Boulder bypasses the active-trap filter.

```{r, per_source_clean}
trap_keep_df <- if (isTRUE(cfg$filter_active)) {
  read.csv(cfg$fn_trap_keep) %>% dplyr::filter(active == 1)
} else {
  read.csv(cfg$fn_trap_keep)
}

clean_one <- function(df_raw, src) {
  if (nrow(df_raw) == 0L) return(tibble::tibble(
    key = character(), trap_id = character(), zone = character(),
    zone2 = character(), year = integer(), week = integer(),
    trap_date = as.Date(character()), method = character(),
    spp = character(), trap_status = character(), total = numeric()
  ))

  prepped <- df_raw %>%
    key_rename(keychain, drop_extra = TRUE) %>%
    wnv_s_clean() %>%
    make_key(key_cols = c("trap_id", "spp", "year", "week")) %>%
    culex_dedup() %>%
    prep_for_skeleton(cfg$spp_levels)

  if (isTRUE(src$filter_active)) {
    prepped <- filter_culex_sheet(
      df            = prepped,
      na_col        = rlang::sym(cfg$na_col),
      trap_keep_df  = trap_keep_df
    )
  }
  prepped
}

sources_prepped <- purrr::imap(sources_raw, ~ clean_one(.x, cfg$sources[[.y]]))
purrr::iwalk(sources_prepped, ~ cat(sprintf("prepped %-12s rows: %d\n", .y, nrow(.x))))

# Combined view (for diagnostics / save) — equivalent to the old culex_clean
culex_clean <- dplyr::bind_rows(sources_prepped, .id = "source")
write.csv(culex_clean, cfg$output_trap_all, row.names = FALSE)
```
````


- [ ] **Step 2: Update the JOIN INTO SKELETON chunk to use the new sources list**

Replace the body of the `{r, join_skeleton}` chunk (lines 311–326) with:

```r
# Priority order: VDCI -> CMC weekly -> CFC -> BC -> database.
# Each list name becomes the value of the `source` column in the output.
data_sources <- list(
  vdci       = sources_prepped$vdci,
  cmc_weekly = sources_prepped$cmc_weekly,
  cfc        = sources_prepped$cfc,
  bc         = sources_prepped$bc,
  database   = database_reformat_prepped
)
# Drop any sources that returned zero rows so fill_skeleton's column check
# doesn't trip on empty frames.
data_sources <- purrr::keep(data_sources, ~ nrow(.x) > 0L)

culex_database_expand <- fill_skeleton(trap_skeleton, data_sources)

unmerged <- collect_unmerged(trap_skeleton, data_sources)
write.csv(unmerged, cfg$mid_unmerged_data, row.names = FALSE)
purrr::iwalk(split(unmerged, unmerged$source),
             ~ cat(sprintf("Unmerged %-12s rows: %d\n", .y, nrow(.x))))
```

- [ ] **Step 3: Render the QMD to verify it parses**

Run:
```bash
Rscript -e 'rmarkdown::render("pipelines/pipeline_combine_culex_sheet.qmd", quiet = TRUE)' 2>&1 | tail -40
```
Expected: render completes, prints per-source row counts and the validation `stopifnot()` messages. If a stopifnot fires, that is investigation territory — DO NOT silence it.

- [ ] **Step 4: Commit**

```bash
git add pipelines/pipeline_combine_culex_sheet.qmd
git commit -m "feat: drive culex pipeline from per-source ingestion"
```

---

## Task 9: Add the completeness validation

**Goal:** Per Toby's constraint (Q12): every Pipiens/Tarsalis trap-week from any source's raw input must appear in either the final `culex_database_expand` or `unmerged_data.csv`. No silent drops.

**Files:**
- Modify: `pipelines/pipeline_combine_culex_sheet.qmd` (the `{r, post_expand_status}` chunk, after the existing five `stopifnot()`s)

- [ ] **Step 1: Append the completeness assertion**

After validation #5 (line ~436) and before `write.csv(culex_database_expand, ...)`, insert:

```r
# 6. Completeness — every Pipiens/Tarsalis row from every source's prepped
#    input must appear in either culex_database_expand or unmerged. A drop
#    here means a record was silently lost between prep and join.
all_prepped_keys <- sources_prepped %>%
  dplyr::bind_rows(.id = "src") %>%
  dplyr::filter(spp %in% cfg$spp_levels) %>%
  dplyr::pull(key) %>%
  unique()

accounted_keys <- unique(c(
  culex_database_expand$key,
  unmerged$key
))

missing_keys <- setdiff(all_prepped_keys, accounted_keys)

cat(
  "\nValidation — completeness:\n",
  "  prepped keys:    ", length(all_prepped_keys), "\n",
  "  accounted keys:  ", length(accounted_keys), "\n",
  "  missing keys:    ", length(missing_keys), "\n"
)
stopifnot(
  "Source prepped records dropped silently between prep and skeleton join" =
    length(missing_keys) == 0L
)
```

- [ ] **Step 2: Re-render the QMD to confirm the new assertion passes**

Run:
```bash
Rscript -e 'rmarkdown::render("pipelines/pipeline_combine_culex_sheet.qmd", quiet = TRUE)' 2>&1 | tail -20
```
Expected: "Validation — completeness: ... missing keys: 0".

- [ ] **Step 3: Commit**

```bash
git add pipelines/pipeline_combine_culex_sheet.qmd
git commit -m "feat: assert no source records dropped between prep and skeleton join"
```

---

## Task 10: Validate against the existing `all_mosq`-derived outputs for 2020–2024

**Goal:** Confirm the new multi-source build reproduces the old VDCI-only build for the overlap window (2020–2024). This is the empirical proof of equivalence.

**Files:**
- Create: `tests/integration/validate_against_all_mosq.R`

- [ ] **Step 1: Save the new output to a separate filename for comparison**

In `config/config_culex_combine.yml`, temporarily set:
```yaml
  output_culex_database_expand: "3_output/culex_sheet_database_expand_NEW.csv"
```
…or simply copy the pre-change file aside before re-rendering:
```bash
cp 3_output/culex_sheet_database_expand.csv 3_output/culex_sheet_database_expand_OLD.csv
```
Pick whichever is more convenient — the rest of this task assumes `_OLD.csv` is the pre-change snapshot and `culex_sheet_database_expand.csv` is the post-change output.

- [ ] **Step 2: Write the comparison script**

```r
# tests/integration/validate_against_all_mosq.R
library(dplyr)
library(testthat)

old <- read.csv("3_output/culex_sheet_database_expand_OLD.csv",
                stringsAsFactors = FALSE) %>%
  dplyr::filter(year %in% 2020:2024) %>%
  dplyr::select(trap_id, year, week, spp, trap_status, total) %>%
  dplyr::arrange(trap_id, year, week, spp)

new <- read.csv("3_output/culex_sheet_database_expand.csv",
                stringsAsFactors = FALSE) %>%
  dplyr::filter(year %in% 2020:2024) %>%
  dplyr::select(trap_id, year, week, spp, trap_status, total) %>%
  dplyr::arrange(trap_id, year, week, spp)

# 1. Row counts
cat("OLD rows (2020-2024):", nrow(old), "\n")
cat("NEW rows (2020-2024):", nrow(new), "\n")

# 2. Set of keys
old_keys <- paste(old$trap_id, old$year, old$week, old$spp, sep = "|")
new_keys <- paste(new$trap_id, new$year, new$week, new$spp, sep = "|")
cat("Keys only in OLD:", length(setdiff(old_keys, new_keys)), "\n")
cat("Keys only in NEW:", length(setdiff(new_keys, old_keys)), "\n")

# 3. Joined per-row diffs
joined <- dplyr::inner_join(
  old %>% dplyr::rename(total_old = total, status_old = trap_status),
  new %>% dplyr::rename(total_new = total, status_new = trap_status),
  by = c("trap_id", "year", "week", "spp")
)

total_diffs <- joined %>%
  dplyr::filter(!(is.na(total_old) & is.na(total_new)),
                is.na(total_old) != is.na(total_new) | total_old != total_new)

status_diffs <- joined %>%
  dplyr::filter(status_old != status_new)

cat("Rows with differing total:", nrow(total_diffs), "\n")
cat("Rows with differing trap_status:", nrow(status_diffs), "\n")

# Print top mismatches for inspection
if (nrow(total_diffs) > 0) print(head(total_diffs, 20))
if (nrow(status_diffs) > 0) print(head(status_diffs, 20))

# Hard assertion only if you expect exact parity. For initial review keep
# this commented out and inspect the printouts first.
# stopifnot(nrow(total_diffs) == 0, nrow(status_diffs) == 0)
```

- [ ] **Step 3: Run the comparison**

Run:
```bash
Rscript tests/integration/validate_against_all_mosq.R
```
Expected output to inspect: row counts and per-row diffs printed; any deltas explained by source-priority differences (e.g., CMC weekly winning over the previously-bundled `all_mosq` numbers when they disagree).

- [ ] **Step 4: Triage discrepancies with Toby before deciding to stop or proceed**

This is a checkpoint — not an automated assertion. If diffs exist, document them in `docs/superpowers/plans/2026-05-20-multi-source-culex-ingestion.md` under a "Validation results" section and consult Toby. Do not silently relax the comparison.

- [ ] **Step 5: Commit the validation script**

```bash
git add tests/integration/validate_against_all_mosq.R
git commit -m "test: add 2020-2024 parity check against all_mosq baseline"
```

---

## Task 11: Retire the obsolete `read_list()` path and old `pattern` key

**Goal:** Remove dead code once the new path is proven. Keep `R/fun_read_list.R` (other code might still source it) but remove the top-level `pattern:` key from the config so future runs cannot accidentally fall back to the old behavior.

**Files:**
- Modify: `config/config_culex_combine.yml`

- [ ] **Step 1: Remove the obsolete top-level keys**

Delete from `config/config_culex_combine.yml`:
```yaml
  pattern: "^(LC).*all mosquitoes.(xlsx|csv)$"
```
and the `culex:` profile block at the bottom (lines 37–38) — it references the same dead pattern.

- [ ] **Step 2: Render the pipeline one more time to confirm nothing depends on the removed keys**

Run:
```bash
Rscript -e 'rmarkdown::render("pipelines/pipeline_combine_culex_sheet.qmd", quiet = TRUE)' 2>&1 | tail -20
```
Expected: clean render, all six `stopifnot()`s pass.

- [ ] **Step 3: Commit**

```bash
git add config/config_culex_combine.yml
git commit -m "chore: retire all_mosq pattern config now that per-source is live"
```

---

## Task 12: Manifest — bulk import inventory and row-conservation audit

**Goal:** Produce a single human-readable log at `3_output/_manifest.txt` that records (a) the bulk import inventory — folders scanned, files matched, and any `Week WW` / `wWW` subfolder under a `WNV-s YYYY*` parent that lacked a matching trap file — and (b) per-source row counts at every pipeline stage so every input row is accounted for as either kept, removed (by dedup / filter / prep), unmerged, or final.

This is the audit complement to the `stopifnot()` assertions: the assertions catch silent drops, the manifest documents the legitimate ones.

**Files:**
- Create: `R/fun_manifest.R`
- Test: `tests/testthat/test-manifest.R`
- Modify: `pipelines/pipeline_combine_culex_sheet.qmd` (sprinkle manifest calls through the per-source chain)

- [ ] **Step 1: Write the failing tests for the manifest helpers**

```r
# tests/testthat/test-manifest.R
test_that("manifest_init creates file with header", {
  tmp <- tempfile(fileext = ".txt")
  manifest_init(tmp)
  txt <- readLines(tmp)
  expect_true(any(grepl("MANIFEST", txt)))
  expect_true(any(grepl("Generated:", txt)))
})

test_that("manifest_log appends without truncating", {
  tmp <- tempfile(fileext = ".txt")
  manifest_init(tmp)
  manifest_log(tmp, "vdci read", n_files = 41, n_rows = 12345)
  manifest_log(tmp, "vdci clean", n_rows = 12300)
  txt <- readLines(tmp)
  expect_true(any(grepl("vdci read", txt)))
  expect_true(any(grepl("vdci clean", txt)))
  expect_true(any(grepl("12345", txt)))
})

test_that("inventory_week_folders flags week folders missing a matched file", {
  tmp <- tempfile(); dir.create(tmp)
  dir.create(file.path(tmp, "WNV-s 2020 (X)", "Week 24", "Data from VDCI"),
             recursive = TRUE)
  dir.create(file.path(tmp, "WNV-s 2020 (X)", "Week 25"), recursive = TRUE)
  file.create(file.path(tmp, "WNV-s 2020 (X)", "Week 24", "Data from VDCI",
                        "LC Week24_2020_Culex.csv"))
  result <- inventory_week_folders(
    root    = tmp,
    pattern = "LC Week.*Culex\\.csv$"
  )
  expect_equal(result$n_year_folders, 1L)
  expect_equal(result$n_week_folders, 2L)
  expect_equal(result$n_matched, 1L)
  expect_equal(length(result$missing_folders), 1L)
  expect_true(grepl("Week 25", result$missing_folders))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-manifest.R")'`
Expected: 3 errors, "could not find function 'manifest_init'".

- [ ] **Step 3: Implement the manifest helpers**

```r
# R/fun_manifest.R
#' Initialise / Truncate the Pipeline Manifest File
#'
#' Writes a fresh header to the manifest. Subsequent manifest_log() calls
#' append. Intended to be called once at the top of the pipeline so a stale
#' manifest from a previous run never bleeds into the current one.
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
#' Writes one block per call. Named arguments become "  <name>: <value>" lines
#' under the label. Use this for any row-count or diagnostic checkpoint.
#'
#' @param path Character. Manifest file path.
#' @param label Character. Section heading for this entry.
#' @param ... Named arguments. Each becomes one indented line.
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
  cat(paste(lines, collapse = "\n"), "\n")  # also echo to console
  invisible(path)
}

#' Inventory Week-Level Folders Under WNV-s Year Folders
#'
#' Walks `root`, finds each `WNV-s YYYY*` year folder, then finds each
#' `Week WW` or `wWW` subfolder. Reports how many of those week folders
#' contain at least one file matching `pattern` (recursively under the
#' week folder).
#'
#' @param root Character. Root directory holding `WNV-s YYYY*` folders.
#' @param pattern Character. Regex for the target trap file (e.g. VDCI's
#'   `"LC Week.*Culex\\.csv$"`).
#' @return Named list:
#'   - n_year_folders: count of `WNV-s YYYY*` directories
#'   - n_week_folders: count of `Week WW` / `wWW` subdirectories
#'   - n_matched: count of week folders containing a matched file
#'   - missing_folders: character vector of week folders WITHOUT a match
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
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-manifest.R")'`
Expected: 3 passes.

- [ ] **Step 5: Add manifest path to config**

In `config/config_culex_combine.yml`, under `default:` add:
```yaml
  manifest: "3_output/_manifest.txt"
```

- [ ] **Step 6: Wire manifest calls into the QMD**

In `pipelines/pipeline_combine_culex_sheet.qmd`:

(a) In the SOURCE USED FUNCTIONS block, add:
```r
source("R/fun_manifest.R")
```

(b) Immediately after the `cfg <- config::get(...)` and `input = ...` lines in the CONFIG chunk, initialise the manifest:
```r
manifest_init(cfg$manifest)
manifest_log(cfg$manifest, "config",
             config_file = "config/config_culex_combine.yml",
             year_start  = cfg$year_start,
             week_range  = paste(cfg$week_start, "-", cfg$week_end),
             spp_levels  = cfg$spp_levels)
```

(c) In the `{r, per_source_import}` chunk, after building `sources_raw`, run inventory and log per source:
```r
# Bulk import inventory: per source, count year folders / week folders /
# files matched / week folders missing a file.
purrr::iwalk(cfg$sources, function(src, src_name) {
  # Only WNV-s yearly subfolders are inventoried (CFC has its own root).
  inv_root <- if (grepl("WNV-s", src$path, fixed = FALSE)) src$path else "1_input/culex_sheet"
  inv <- inventory_week_folders(inv_root, src$pattern)
  manifest_log(cfg$manifest, paste0("inventory:", src_name),
               root            = inv_root,
               pattern         = src$pattern,
               n_year_folders  = inv$n_year_folders,
               n_week_folders  = inv$n_week_folders,
               n_matched_files = inv$n_matched,
               n_missing       = length(inv$missing_folders),
               missing_folders = if (length(inv$missing_folders) == 0L) "(none)"
                                 else inv$missing_folders)
})

# Raw row counts per source.
purrr::iwalk(sources_raw, function(df, src_name) {
  manifest_log(cfg$manifest, paste0("read:", src_name),
               n_rows = nrow(df), n_cols = ncol(df))
})
```

(d) Replace the `clean_one()` definition in `{r, per_source_clean}` with an instrumented version that logs each stage's row count to the manifest. The conservation equation is logged per source at the end:

```r
clean_one <- function(df_raw, src, src_name) {
  if (nrow(df_raw) == 0L) {
    manifest_log(cfg$manifest, paste0("clean:", src_name),
                 status = "EMPTY — skipped")
    return(tibble::tibble(
      key = character(), trap_id = character(), zone = character(),
      zone2 = character(), year = integer(), week = integer(),
      trap_date = as.Date(character()), method = character(),
      spp = character(), trap_status = character(), total = numeric()
    ))
  }

  n_raw  <- nrow(df_raw)
  after_clean  <- df_raw %>% key_rename(keychain, drop_extra = TRUE) %>% wnv_s_clean()
  n_clean      <- nrow(after_clean)
  after_key    <- after_clean %>% make_key(key_cols = c("trap_id", "spp", "year", "week"))
  after_dedup  <- after_key %>% culex_dedup()
  n_dedup      <- nrow(after_dedup)
  after_prep   <- after_dedup %>% prep_for_skeleton(cfg$spp_levels)
  n_prep       <- nrow(after_prep)

  if (isTRUE(src$filter_active)) {
    after_filter <- filter_culex_sheet(
      df            = after_prep,
      na_col        = rlang::sym(cfg$na_col),
      trap_keep_df  = trap_keep_df
    )
  } else {
    after_filter <- after_prep
  }
  n_filter <- nrow(after_filter)

  manifest_log(cfg$manifest, paste0("clean:", src_name),
               n_raw                = n_raw,
               n_after_wnv_s_clean  = n_clean,
               n_after_culex_dedup  = n_dedup,
               n_after_prep         = n_prep,
               n_after_filter       = n_filter,
               removed_by_dedup     = n_clean - n_dedup,
               net_change_at_prep   = n_prep - n_dedup,
               removed_by_filter    = n_prep - n_filter)
  after_filter
}

sources_prepped <- purrr::imap(sources_raw, ~ clean_one(.x, cfg$sources[[.y]], .y))
```

(e) After the `fill_skeleton()` call in the `{r, join_skeleton}` chunk, append per-source matched / unmerged accounting:

```r
purrr::iwalk(sources_prepped, function(df, src_name) {
  if (nrow(df) == 0L) return()
  n_in  <- nrow(df)
  n_unm <- sum(unmerged$source == src_name, na.rm = TRUE)
  n_exp <- sum(culex_database_expand$source == src_name, na.rm = TRUE)
  manifest_log(cfg$manifest, paste0("join:", src_name),
               n_into_skeleton    = n_in,
               n_in_final_expand  = n_exp,
               n_unmerged         = n_unm,
               balance_check      = paste0(n_in, " == ", n_exp, " + ", n_unm,
                                           " : ",
                                           if (n_in == n_exp + n_unm) "OK"
                                           else paste0("DRIFT (",
                                                       n_in - n_exp - n_unm, ")")))
})
```

(f) After the existing six `stopifnot()` blocks in `{r, post_expand_status}`, append a final manifest summary block:

```r
manifest_log(cfg$manifest, "final",
             skeleton_rows         = nrow(trap_skeleton),
             expand_rows           = nrow(culex_database_expand),
             unmerged_rows         = nrow(unmerged),
             malfunction_trap_wks  = n_final_malfunction,
             no_mosq_trap_wks      = n_final_no_mosq,
             no_culex_trap_wks     = n_final_no_culex)
```

- [ ] **Step 7: Render the pipeline and inspect the manifest**

Run:
```bash
Rscript -e 'rmarkdown::render("pipelines/pipeline_combine_culex_sheet.qmd", quiet = TRUE)'
cat 3_output/_manifest.txt
```
Expected: a manifest containing the config block, four inventory blocks (one per source), four read blocks, four clean blocks, four join blocks (each with `balance_check: ... : OK`), and a final summary. Any `DRIFT` value in a `balance_check` line is a real problem — investigate before signing off.

- [ ] **Step 8: Commit**

```bash
git add R/fun_manifest.R tests/testthat/test-manifest.R config/config_culex_combine.yml pipelines/pipeline_combine_culex_sheet.qmd
git commit -m "feat: write per-source bulk-import and row-conservation manifest"
```

---

## Critical Files

| File | Action | Why |
|---|---|---|
| `R/fun_read_source.R` | Create | Per-source dispatcher (Task 4) |
| `R/fun_pivot_cmc.R` | Create | CMC weekly wide→long (Task 5) |
| `R/fun_pivot_cfc.R` | Create | CFC 2006-2017 wide→long (Task 6) |
| `R/fun_pivot_boulder.R` | Create | Boulder normalization (Task 7) |
| `R/fun_manifest.R` | Create | Manifest helpers + week-folder inventory (Task 12) |
| `tests/testthat/test-read_source.R` | Create | Unit tests for Task 4 |
| `tests/testthat/test-pivot_cmc.R` | Create | Unit tests for Task 5 |
| `tests/testthat/test-pivot_cfc.R` | Create | Unit tests for Task 6 |
| `tests/testthat/test-pivot_boulder.R` | Create | Unit tests for Task 7 |
| `tests/testthat/test-manifest.R` | Create | Unit tests for Task 12 |
| `tests/integration/validate_against_all_mosq.R` | Create | Parity check (Task 10) |
| `1_input/database_column_rename.csv` | Modify | Source column aliases (Task 2) |
| `config/config_culex_combine.yml` | Modify | `sources:` block + manifest path + retirement (Tasks 3, 11, 12) |
| `pipelines/pipeline_combine_culex_sheet.qmd` | Modify | Per-source loop + completeness assertion + manifest wiring (Tasks 8, 9, 12) |
| `3_output/_manifest.txt` | Generated | Run-time audit log — written by manifest helpers, gitignored |
| `docs/source_file_inventory.md` | Create | Inventory note (Task 1) |

---

## Reuse — existing functions left untouched

These already exist and are called as-is from the new path:

| Function | File | Role in new flow |
|---|---|---|
| `key_rename()` | `R/key_rename.R` | Alias translation (extended via Task 2) |
| `wnv_s_clean()` | `R/fun_wnv_s_clean.R` | Canonical cleaning (already handles species/status logic) |
| `make_key()` | `R/fun_make_key.R` | Builds the join key |
| `culex_dedup()` | `R/fun_dedup_culex.R` | Removes weekend collection nights |
| `prep_for_skeleton()` | `R/fun_prep_for_skeleton.R` | Shapes a source for `fill_skeleton()` |
| `filter_culex_sheet()` | `R/fun_filter_culex_sheet.R` | Active-trap filter per source |
| `expand_trap()` | `R/fun_expand_trap.R` | Builds the skeleton |
| `fill_skeleton()` | `R/fun_fill_skeleton.R` | Priority-order join (now driven from a 5-item list) |
| `collect_unmerged()` | `R/fun_fill_skeleton.R` | Audit trail for unmerged records |
| `assign_trap_status_post_expand()` | `R/fun_assign_trap_status.R` | Resolves `no trap` vs `no mosquitoes` |
| `reformat_database_2_culex_sheet()` | `R/fun_reformat_database_2_culex_sheet.R` | Database supplement (unchanged) |
| `fix_zone_from_ref()` | `R/fun_fix_zone.R` | Zone correction from `foco_trap` ref |

---

## Verification

End-to-end smoke test (run at the end of every task that touches code):

```bash
# 1. Unit tests for all new functions
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'

# 2. Full pipeline render
Rscript -e 'rmarkdown::render("pipelines/pipeline_combine_culex_sheet.qmd", quiet = TRUE)'

# 3. Parity check against all_mosq baseline (after Task 10 exists)
Rscript tests/integration/validate_against_all_mosq.R

# 4. Inspect the run manifest (after Task 12 exists)
cat 3_output/_manifest.txt
```

Expected at completion:
- All unit tests pass.
- Pipeline render prints six `Validation —` blocks, each ending in a count that satisfies the `stopifnot()`.
- Parity check shows zero `setdiff` keys for 2020–2024, zero `total` diffs, zero `trap_status` diffs (or any diffs are explained and signed off by Toby).
- `3_output/_manifest.txt` contains: a config block, one `inventory:<src>` block per source (with `n_missing: 0` ideally, or named missing folders documented), one `read:<src>` / `clean:<src>` / `join:<src>` block per source — every `balance_check` line ends in `OK`, never `DRIFT`. The final block summarises skeleton / expand / unmerged counts.
