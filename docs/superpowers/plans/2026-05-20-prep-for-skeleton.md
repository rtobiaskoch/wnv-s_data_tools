# `prep_for_skeleton()` — Clean Source Shaping Before Skeleton Join

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Create `prep_for_skeleton()`, which converts any cleaned culex source (mixed spp types) into a form where every row has `spp ∈ spp_levels` and `trap_status` is already correctly assigned — so `fill_skeleton()` receives clean, consistently shaped data and no post-hoc propagation is needed after the join.

**Architecture:** `prep_for_skeleton()` splits input rows on `trap_status` (culex vs. all other statuses), expands each group to cover all `spp_levels`, and rebuilds the key. It is called on each source *before* `filter_culex_sheet()` so that status rows survive to enter the skeleton rather than being dropped by the species filter or silently missing due to key mismatch. The post-hoc malfunction propagation block added to the pipeline is removed — it is superseded by this approach.

**Naming note:** `trap_status = "no culex"` is the status label used throughout. The `spp` column from `wnv_s_clean()` uses `"non culex"` as the cleaned spp value — this is a different column from `trap_status`. `prep_for_skeleton()` splits on `trap_status`, so the input `spp` value does not drive any logic in this function.

**Tech Stack:** R, dplyr, tidyr, testthat

---

## Context

`fill_skeleton()` joins each source into the skeleton on a key = `(trap_id, spp, year, week)`. The skeleton only has `spp ∈ spp_levels` (Tarsalis, Pipiens). Three status types currently never reach the skeleton correctly:

| Status | spp after cleaning | Problem |
|--------|-------------------|---------|
| `"no culex"` | `"non culex"` | Dropped by `filter_culex_sheet()` line 65 (`spp != "non culex"`) |
| `"malfunction"` | `"none"` | Key mismatch — `spp="none"` not in skeleton |
| `"no mosquitoes"` (explicit) | `"none"` | Key mismatch — zone-week may become `"no trap"` if all traps were explicit no-mosquitoes |

The malfunction case was patched post-hoc in the pipeline (commit `64d9631`). That patch is removed by this plan.

---

## File Map

| Action | File | Purpose |
|--------|------|---------|
| Create | `R/fun_prep_for_skeleton.R` | `prep_for_skeleton(df, spp_levels)` — pure shaping function |
| Create | `tests/testthat/test-prep_for_skeleton.R` | TDD tests for all four input cases |
| Modify | `pipelines/pipeline_combine_culex_sheet.qmd` | Add `prep_for_skeleton()` call before filter; apply to database_reformat; remove post-hoc malfunction block; add "no culex" validator; add source() |

`R/fun_filter_culex_sheet.R` — **no changes**. After `prep_for_skeleton()` runs, all rows entering `filter_culex_sheet()` have `spp ∈ spp_levels`, so the `spp != "non culex"` filter on line 65 becomes a harmless no-op.

---

## Task 1: Create `prep_for_skeleton()` [TDD]

**Files:**
- Create: `R/fun_prep_for_skeleton.R`
- Create: `tests/testthat/test-prep_for_skeleton.R`

- [ ] **Step 1: Write failing tests**

Create `tests/testthat/test-prep_for_skeleton.R`:

```r
library(testthat)
source(here::here("R/fun_prep_for_skeleton.R"))
source(here::here("R/fun_make_key.R"))

# Minimal helper — a single cleaned culex observation as wnv_s_clean() would produce.
# NOTE on spp vs trap_status:
#   trap_status = "no culex"  (the status label — used consistently throughout)
#   spp         = "non culex" (the cleaned spp column value wnv_s_clean() produces)
# prep_for_skeleton() splits on trap_status, NOT spp, so the input spp value
# does not affect the function's logic. Tests use "no culex" for spp in status
# tests for readability; real data would have "non culex" from wnv_s_clean().
make_obs <- function(spp, trap_status, total,
                     trap_id = "FC-001", zone = "NE", year = 2023L,
                     week = 25L, trap_date = "2023-06-19", method = "L") {
  data.frame(
    trap_id     = trap_id,
    zone        = zone,
    year        = year,
    week        = week,
    trap_date   = as.Date(trap_date),
    method      = method,
    spp         = spp,
    trap_status = trap_status,
    total       = as.numeric(total),
    stringsAsFactors = FALSE
  )
}

# ── Output shape ──────────────────────────────────────────────────────────────
test_that("output spp values are restricted to spp_levels only", {
  df <- rbind(
    make_obs("Tarsalis",  "culex",         10),
    make_obs("none",      "malfunction",    0),
    make_obs("no culex", "no culex",       5),
    make_obs("none",      "no mosquitoes",  0)
  )
  result <- prep_for_skeleton(df, c("Tarsalis", "Pipiens"))
  expect_true(all(result$spp %in% c("Tarsalis", "Pipiens")))
})

test_that("output has a key column derived from trap_id, spp, year, week", {
  result <- prep_for_skeleton(make_obs("Tarsalis", "culex", 10),
                              c("Tarsalis", "Pipiens"))
  expect_true("key" %in% names(result))
  expect_true(all(!is.na(result$key)))
})

# ── Culex catches: species completion ─────────────────────────────────────────
test_that("culex: single-spp catch expanded to both spp; missing spp gets total = 0", {
  result <- prep_for_skeleton(make_obs("Tarsalis", "culex", 10),
                              c("Tarsalis", "Pipiens"))
  expect_equal(nrow(result), 2L)
  expect_equal(result$total[result$spp == "Tarsalis"], 10)
  expect_equal(result$total[result$spp == "Pipiens"],   0)
  expect_true(all(result$trap_status == "culex"))
})

test_that("culex: both spp already present — totals preserved, no duplication", {
  df <- rbind(
    make_obs("Tarsalis", "culex", 10),
    make_obs("Pipiens",  "culex",  3)
  )
  result <- prep_for_skeleton(df, c("Tarsalis", "Pipiens"))
  expect_equal(nrow(result), 2L)
  expect_equal(result$total[result$spp == "Tarsalis"], 10)
  expect_equal(result$total[result$spp == "Pipiens"],   3)
})

# ── Status events: expansion ───────────────────────────────────────────────────
test_that("malfunction: expanded to both spp with total = NA", {
  result <- prep_for_skeleton(make_obs("none", "malfunction", 0),
                              c("Tarsalis", "Pipiens"))
  expect_equal(nrow(result), 2L)
  expect_equal(sort(result$spp), c("Pipiens", "Tarsalis"))
  expect_true(all(result$trap_status == "malfunction"))
  expect_true(all(is.na(result$total)))
})

test_that("no culex: expanded to both spp with total = 0", {
  result <- prep_for_skeleton(make_obs("no culex", "no culex", 5),
                              c("Tarsalis", "Pipiens"))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$trap_status == "no culex"))
  expect_true(all(result$total == 0))
})

test_that("no mosquitoes: expanded to both spp with total = 0", {
  result <- prep_for_skeleton(make_obs("none", "no mosquitoes", 0),
                              c("Tarsalis", "Pipiens"))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$trap_status == "no mosquitoes"))
  expect_true(all(result$total == 0))
})

# ── Mixed input ───────────────────────────────────────────────────────────────
test_that("mixed input: row count correct across all status types", {
  # 1 culex trap → 2 rows; 1 malfunction → 2 rows; 1 no culex → 2 rows
  df <- rbind(
    make_obs("Tarsalis",  "culex",        10, trap_id = "FC-001"),
    make_obs("none",      "malfunction",   0, trap_id = "FC-002"),
    make_obs("no culex", "no culex",      5, trap_id = "FC-003")
  )
  result <- prep_for_skeleton(df, c("Tarsalis", "Pipiens"))
  expect_equal(nrow(result), 6L)
})

test_that("no rows in output with spp outside spp_levels after mixed input", {
  df <- rbind(
    make_obs("Tarsalis",  "culex",         10),
    make_obs("none",      "malfunction",    0),
    make_obs("no culex", "no culex",       5),
    make_obs("none",      "no mosquitoes",  0)
  )
  result <- prep_for_skeleton(df, c("Tarsalis", "Pipiens"))
  expect_false(any(result$spp %in% c("none", "non culex")))
})

# ── Total coercion rules ──────────────────────────────────────────────────────
# These are rule tests, not just incidental checks on expected input values:
#   malfunction | no trap  → total must be NA  (trap did not collect data)
#   no culex | no mosquitoes → total must be 0  (trap ran, nothing relevant caught)
# "no trap" is set downstream by assign_trap_status_post_expand(), not by
# prep_for_skeleton() — it is tested by the pipeline validator in Task 2 Step 6.

test_that("malfunction: total is NA even when input total is non-zero", {
  result <- prep_for_skeleton(make_obs("no culex", "malfunction", 99),
                              c("Tarsalis", "Pipiens"))
  expect_true(all(is.na(result$total)))
})

test_that("no culex: total is 0 even when input total is non-zero", {
  result <- prep_for_skeleton(make_obs("no culex", "no culex", 99),
                              c("Tarsalis", "Pipiens"))
  expect_true(all(result$total == 0))
  expect_false(any(is.na(result$total)))
})

test_that("no mosquitoes: total is 0 even when input total is non-zero", {
  result <- prep_for_skeleton(make_obs("no culex", "no mosquitoes", 99),
                              c("Tarsalis", "Pipiens"))
  expect_true(all(result$total == 0))
  expect_false(any(is.na(result$total)))
})
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
cd /Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner
Rscript -e "testthat::test_file('tests/testthat/test-prep_for_skeleton.R')"
```

Expected: FAIL — `prep_for_skeleton` not found.

- [ ] **Step 3: Implement `prep_for_skeleton()`**

Create `R/fun_prep_for_skeleton.R`:

```r
library(dplyr)
library(tidyr)

#' Shape Cleaned Source Data for Skeleton Join
#'
#' Converts cleaned culex surveillance data (which may contain spp values of
#' "Tarsalis", "Pipiens", "none", or "non culex" from wnv_s_clean()) into a
#' form where every row has spp ∈ spp_levels and trap_status is already set.
#'
#' This means fill_skeleton() receives clean, consistently shaped data from any
#' source. No post-hoc propagation of status rows is needed after the join.
#'
#' Four input cases are handled:
#'   "culex":         expand to all spp_levels; missing species gets total = 0
#'   "malfunction":   expand to all spp_levels; total = NA
#'   "no culex":      expand to all spp_levels; total = 0
#'   "no mosquitoes": expand to all spp_levels; total = 0
#'
#' wnv_s_clean() assigns trap_status at the trap-date group level (via
#' group_by(trap_id, trap_date)), so all rows for a trap-date share the same
#' status. The culex vs. status split on trap_status is therefore clean —
#' a trap-date is either entirely "culex" or entirely a status event.
#'
#' @param df        Cleaned data frame from wnv_s_clean(). Must contain:
#'   trap_id, zone, year, week, trap_date, method, spp, trap_status, total.
#' @param spp_levels Character vector of species to expand to (from cfg$spp_levels).
#'
#' @return Data frame with spp ∈ spp_levels only and a rebuilt key column.
#'   Columns: key, trap_id, zone, year, week, trap_date, method, spp,
#'   trap_status, total.
#' @export
prep_for_skeleton <- function(df, spp_levels) {

  # Split on trap_status: culex catches vs. all status events.
  # All rows in a trap-date group share the same trap_status (wnv_s_clean
  # group assigns), so this split is mutually exclusive.
  culex_rows  <- df %>% dplyr::filter(trap_status == "culex")
  status_rows <- df %>% dplyr::filter(trap_status != "culex")

  # ── culex catches: complete the species dimension ──────────────────────────
  # For each trap-date where culex was caught, produce one row per spp_level.
  # Join actual totals back — missing species (not caught) gets total = 0.
  culex_complete <- if (nrow(culex_rows) > 0L) {
    trap_meta <- culex_rows %>%
      dplyr::distinct(trap_id, zone, year, week, trap_date, method, trap_status)

    trap_meta %>%
      tidyr::crossing(spp = spp_levels) %>%
      dplyr::left_join(
        culex_rows %>% dplyr::select(trap_id, year, week, spp, total),
        by = c("trap_id", "year", "week", "spp")
      ) %>%
      dplyr::mutate(total = dplyr::coalesce(as.numeric(total), 0))
  } else {
    dplyr::tibble()
  }

  # ── status events: expand to all spp_levels ───────────────────────────────
  # malfunction → total = NA (trap did not collect data)
  # no culex / no mosquitoes → total = 0
  status_expanded <- if (nrow(status_rows) > 0L) {
    status_rows %>%
      dplyr::distinct(trap_id, zone, year, week, trap_date, method, trap_status) %>%
      tidyr::crossing(spp = spp_levels) %>%
      dplyr::mutate(
        total = dplyr::if_else(trap_status == "malfunction", NA_real_, 0)
      )
  } else {
    dplyr::tibble()
  }

  dplyr::bind_rows(culex_complete, status_expanded) %>%
    # Rebuild key: spp has changed from "none"/"non culex" to spp_levels values
    make_key(key_cols = c("trap_id", "spp", "year", "week")) %>%
    # Guard against duplicate keys from the same trap having two dates in one
    # week (culex_dedup should prevent this, but defensive dedup is cheap)
    dplyr::distinct(key, .keep_all = TRUE) %>%
    dplyr::select(key, trap_id, zone, year, week, trap_date, method,
                  spp, trap_status, total)
}
```

- [ ] **Step 4: Run tests to confirm they pass**

```bash
Rscript -e "testthat::test_file('tests/testthat/test-prep_for_skeleton.R')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 12 ]`

- [ ] **Step 5: Commit**

```bash
git add R/fun_prep_for_skeleton.R tests/testthat/test-prep_for_skeleton.R
git commit -m "feat: add prep_for_skeleton() — shape source data to spp_levels before skeleton join"
```

---

## Task 2: Wire `prep_for_skeleton()` into the Pipeline

**Files:**
- Modify: `pipelines/pipeline_combine_culex_sheet.qmd`

Four changes to the pipeline QMD, described below.

- [ ] **Step 1: Add `source()` call in the ENVIRONMENT chunk**

After the existing `source("R/fun_fill_skeleton.R")` line (line 37), add:

```r
source("R/fun_prep_for_skeleton.R")    # prep_for_skeleton
```

- [ ] **Step 2: Add `prep_for_skeleton()` call in the CLEAN chunk**

Find the CLEAN chunk. It currently ends with:

```r
culex_clean = culex0 %>%
  key_rename(keychain, drop_extra = TRUE) %>%
  wnv_s_clean() %>%
  make_key(key_cols = c("trap_id", "spp", "year", "week")) %>%
  culex_dedup()
```

Add a `prep_for_skeleton()` call immediately after `culex_clean` is created (before the `culex_clean_dupes_key` lines):

```r
# Convert all spp types to spp_levels rows with trap_status set.
# This runs before filter_culex_sheet() so that status rows ("non culex",
# "none") survive to enter the skeleton rather than being dropped by the
# species filter or failing the key join.
culex_clean_prepped <- prep_for_skeleton(culex_clean, cfg$spp_levels)
```

- [ ] **Step 3: Update the FILTER chunk to use `culex_clean_prepped`**

Find the FILTER chunk. It currently passes `culex_clean` to `filter_culex_sheet()`. Change it to pass `culex_clean_prepped`:

```r
culex_filtered = filter_culex_sheet(df = culex_clean_prepped,
                                    na_col = rlang::sym(cfg$na_col),
                                    trap_keep_df = trap_keep_df)
```

> The `spp != "non culex"` filter inside `filter_culex_sheet()` (line 65) becomes a harmless no-op because all rows entering it now have `spp ∈ spp_levels`. No change to `filter_culex_sheet()` is needed.

- [ ] **Step 4: Apply `prep_for_skeleton()` to `database_reformat` in the REFORMAT chunk**

Find the REFORMAT DATASHEET chunk. It currently ends with `culex_dedup()`. Add a prep call on the next line:

```r
database_reformat_prepped <- prep_for_skeleton(database_reformat, cfg$spp_levels)
```

- [ ] **Step 5: Replace the JOIN INTO SKELETON chunk body**

Find the `join_skeleton` chunk (currently lines ~304–339). Replace its entire body with the version below, which:
- Uses `culex_filtered` (already prepped) and `database_reformat_prepped` in `data_sources`
- Removes the post-hoc malfunction propagation block (no longer needed)

```r
# Priority order: culex sheet first, database fills remaining gaps.
# Each source has been shaped by prep_for_skeleton() — all rows have
# spp ∈ spp_levels with trap_status already set. The skeleton join is
# now a clean key-match with no post-hoc status propagation needed.
data_sources <- list(
  culex_sheet = culex_filtered,
  database    = database_reformat_prepped
)

culex_database_expand <- fill_skeleton(trap_skeleton, data_sources)

unmerged <- collect_unmerged(trap_skeleton, data_sources)
write.csv(unmerged, cfg$mid_unmerged_data, row.names = FALSE)
cat("Unmerged culex_sheet rows:", nrow(unmerged[unmerged$source == "culex_sheet", ]), "\n")
cat("Unmerged database rows:   ", nrow(unmerged[unmerged$source == "database", ]), "\n")
```

- [ ] **Step 6: Add "no culex" validator in the POST-EXPAND STATUS chunk**

Find the `post_expand_status` chunk. After the existing two `stopifnot()` validators (malfunction count and no-mosquitoes count), add a third.

> **Why `culex_filtered`, not `culex_clean` or `culex0`:** All validators compare against `culex_filtered` (active traps, post-filter). `culex_clean` includes inactive traps that are never in the skeleton — comparing against it would count trap-weeks that cannot appear in the output, causing false failures. `culex0` is raw data with no `trap_status` column at all. `culex_filtered` is the correct baseline because it has already been restricted to the same active-trap scope as the skeleton.

```r
# 3. "No culex" trap-week count must be identical in input and final output.
#    Traps that caught only non-culex species contribute to the denominator
#    and must not be silently reclassified as "no mosquitoes" or "no trap".
#    Baseline is culex_filtered (active traps, post-prep_for_skeleton): "no culex"
#    rows exist here as Tarsalis/Pipiens rows with trap_status="no culex".
n_raw_no_culex <- culex_filtered %>%
  dplyr::filter(trap_status == "no culex") %>%
  dplyr::distinct(trap_id, year, week) %>%
  nrow()

n_final_no_culex <- culex_database_expand %>%
  dplyr::filter(trap_status == "no culex") %>%
  dplyr::distinct(trap_id, year, week) %>%
  nrow()

cat(
  "\nValidation — 'no culex' trap-weeks:\n",
  "  culex_filtered input: ", n_raw_no_culex, "\n",
  "  final expanded:       ", n_final_no_culex, "\n"
)
stopifnot(
  "'no culex' trap-week count changed between input and final output" =
    n_final_no_culex == n_raw_no_culex
)

# 4. malfunction and no trap rows must have total = NA.
#    "no trap" is set by assign_trap_status_post_expand(), malfunction by
#    wnv_s_clean(). Both mean no mosquito data was collected.
stopifnot(
  "malfunction and no trap rows must have total = NA" = {
    culex_database_expand %>%
      dplyr::filter(trap_status %in% c("malfunction", "no trap"),
                    !is.na(total)) %>%
      nrow() == 0
  }
)

# 5. no culex and no mosquitoes rows must have total = 0.
#    Trap ran and caught nothing relevant; total must be exactly 0, not NA.
stopifnot(
  "no culex and no mosquitoes rows must have total = 0" = {
    culex_database_expand %>%
      dplyr::filter(trap_status %in% c("no culex", "no mosquitoes"),
                    is.na(total) | total != 0) %>%
      nrow() == 0
  }
)
```

- [ ] **Step 7: Run the full test suite**

```bash
Rscript -e "testthat::test_dir('tests/testthat/')"
```

Expected: all tests pass across all test files.

- [ ] **Step 8: Commit**

```bash
git add pipelines/pipeline_combine_culex_sheet.qmd
git commit -m "refactor: prep_for_skeleton() before filter — remove post-hoc malfunction propagation, add no-culex validator"
```

---

## Verification

After both tasks are complete, run these checks in an R session at the project root:

```r
setwd("/Users/user/Programming_Directory/Ebel_Lab/wnv-ss_trap_hx_combiner")

# 1. All unit tests pass
testthat::test_dir("tests/testthat/")

# 2. culex_clean_prepped has only Tarsalis/Pipiens spp
stopifnot(all(culex_clean_prepped$spp %in% c("Tarsalis", "Pipiens")))

# 3. No "non culex" or "none" spp reach fill_skeleton
stopifnot(!any(culex_filtered$spp %in% c("non culex", "none")))

# 4. Run pipeline through post_expand_status chunk and confirm stopifnot() validators pass:
#    - malfunction count matches
#    - no-mosquitoes count >= input
#    - no-culex count matches
```

## Notes

- The post-hoc malfunction propagation block (added in commit `64d9631`, lines ~315–333 of the pipeline) is **deleted** in Step 5. It is superseded by `prep_for_skeleton()`.
- `filter_culex_sheet()` is **not modified**. Its `spp != "non culex"` filter on line 65 becomes a no-op after `prep_for_skeleton()` runs. If that filter is removed in a future cleanup pass, the behaviour is identical.
- `collect_unmerged()` uses the prepped data. Unmerged records will have `spp ∈ spp_levels` rather than raw spp values — this is consistent with the skeleton's key structure.
- **All three pipeline validators use `culex_clean` as the baseline, not `culex0`.** `culex0` is raw import data with no `trap_status` column — it contains raw strings like "Malfunction", "Trap Stolen", "Trap Vandalized" as individual values, and has no concept of "no culex". `culex_clean` is post-`wnv_s_clean()`, where those variants are already normalised to `trap_status = "malfunction"` and the "no culex" classification already exists. Comparing against `culex_clean` ensures the validator measures what the pipeline actually intends to preserve.
- **"no culex" vs "non culex":** `trap_status = "no culex"` is the status label used everywhere in this pipeline. `spp = "non culex"` is the cleaned spp column value `wnv_s_clean()` produces for non-culex catches. These are different columns. `prep_for_skeleton()` splits on `trap_status`, so the input spp value does not drive any logic.
