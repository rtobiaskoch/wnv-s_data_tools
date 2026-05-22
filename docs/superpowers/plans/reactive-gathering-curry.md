# Plan: Separate clean / prep / filter stages in per-source pipeline

## Context

Two issues from `docs/TODO.md`:

1. **"no culex status dropped"** — trap-weeks with `trap_status == "no culex"` are
   missing from the final `3_output/culex_sheet_database_expand.csv` for cases
   the user expects them to appear.
2. **`2_mid/culex_clean_all_spp_plot.png` only shows Tarsalis/Pipiens** — the
   plot is meant to be the "all species" diagnostic but currently shows only
   the two target species.

Root cause for #2 is structural and the same root cause makes #1 hard to
diagnose: `clean_one()` in `pipelines/pipeline_combine_culex_sheet.qmd`
(chunk `per_source_clean`, ~line 157) does three things in one pass — it
cleans, it preps for the skeleton (`prep_for_skeleton()`, which drops every
species not in `cfg$spp_levels`), and it filters (`filter_culex_sheet()`,
which drops inactive traps). That violates separation of concerns and:

- collapses original species values before `culex_clean` is built, so
  `plot_all_spp(culex_clean, "zone2")` only ever sees Tarsalis/Pipiens, and
- buries the per-stage row counts so we can't see where "no culex" rows are
  dropped (dedup? prep? active-trap filter? skeleton join?).

**Goal:** split `clean_one()` into three discrete stages with their own
chunks and their own manifest logs. Stage 1 (clean) must not drop rows for
analysis reasons. After the restructure, the all-spp plot is correct
automatically and the manifest exposes per-stage row counts so Issue 1 can be
diagnosed from real numbers rather than guessed.

---

## Files to modify

Only `pipelines/pipeline_combine_culex_sheet.qmd` is touched. No R function
in `R/` changes — `prep_for_skeleton()` and `filter_culex_sheet()` are
reused as-is, just moved out of `clean_one()` and into their own pipeline
chunks.

---

## Changes

### 1. Slim down `clean_one()` in chunk `per_source_clean`

`pipelines/pipeline_combine_culex_sheet.qmd` lines ~155–196.

Keep only the canonical cleaning chain:

```
key_rename(drop_extra = TRUE) → wnv_s_clean() → make_key() → culex_dedup()
```

Remove:
- the `prep_for_skeleton()` call
- the `filter_culex_sheet()` call (and the surrounding `if (isTRUE(src$filter_active))` gate)
- the `src` argument to `clean_one()` (no longer needed; only `src_name`
  is used for manifest logging)
- the `trap_keep_df` load at the top of the chunk (moves to step 3)

Return value: cleaned tibble that **still contains all observed species**
(`spp` = "Aedes vexans", "non culex", "Tarsalis", "Pipiens", …) and **all
observed traps** (no active-trap restriction).

`wnv_s_clean()` (`R/fun_wnv_s_clean.R:97`) already saves the raw species
value in a `spp0` column, so downstream code that groups by `spp0`
(`R/fun_plot_all_spp.R:4`) works without any `mutate(spp0 = spp)`.

Manifest log per source becomes:
```
clean:<src>  n_raw, n_after_clean, n_after_dedup,
             removed_by_dedup
```

Rename the result list `sources_cleaned` (was `sources_prepped`):

```r
sources_cleaned <- purrr::imap(sources_raw, ~ clean_one(.x, .y))
```

### 2. Rebuild `culex_clean` from `sources_cleaned`

Same chunk, replace the existing `culex_clean` construction:

```r
culex_clean <- dplyr::bind_rows(sources_cleaned, .id = "source")
write.csv(culex_clean, cfg$output_trap_all, row.names = FALSE)

p_culex_clean <- plot_all_spp(culex_clean, "zone2")
ggsave(cfg$mid_culex_clean_plot, p_culex_clean, width = 12, height = 10)
```

No `mutate(spp0 = spp)` — `spp0` is already populated by `wnv_s_clean()`.
This single change fixes Issue 2.

### 3. New chunk `per_source_prep`

Insert immediately after `per_source_clean`.

```r
sources_prepped <- purrr::imap(sources_cleaned, function(df, src_name) {
  out <- prep_for_skeleton(df, cfg$spp_levels)
  manifest_log(cfg$manifest, paste0("prep:", src_name),
               n_in_clean = nrow(df), n_out_prepped = nrow(out))
  out
})
```

Reuses `prep_for_skeleton()` (`R/fun_prep_for_skeleton.R`) unchanged.

### 4. New chunk `per_source_filter`

Insert immediately after `per_source_prep`. This chunk owns the active-trap
load (moved out of `per_source_clean`) and applies `filter_culex_sheet()`
per source based on each source's `filter_active` flag from
`config/config_culex_combine.yml`:

```r
if (isTRUE(cfg$filter_active)) {
  trap_keep_df <- read.csv(cfg$fn_trap_keep) %>% dplyr::filter(active == 1)
} else {
  trap_keep_df <- read.csv(cfg$fn_trap_keep)
}

sources_filtered <- purrr::imap(sources_prepped, function(df, src_name) {
  src <- cfg$sources[[src_name]]
  out <- if (isTRUE(src$filter_active)) {
    filter_culex_sheet(df, na_col = rlang::sym(cfg$na_col),
                       trap_keep_df = trap_keep_df)
  } else df
  manifest_log(cfg$manifest, paste0("filter:", src_name),
               n_in_prepped = nrow(df), n_out_filtered = nrow(out),
               removed_by_filter = nrow(df) - nrow(out))
  out
})
```

Reuses `filter_culex_sheet()` (`R/fun_filter_culex_sheet.R`) unchanged.

### 5. Update the existing `filter` chunk

`pipeline_combine_culex_sheet.qmd` lines ~266–278. `culex_filtered` is now
built from `sources_filtered` rather than `sources_prepped`:

```r
culex_filtered <- dplyr::bind_rows(
  purrr::keep(sources_filtered, function(df) nrow(df) > 0)
)
```

Everything downstream that consumes `culex_filtered`
(`join_cxsheet_database`, the comparison plot, validations 1–3) is
unchanged.

### 6. Replace `sources_prepped` references downstream

Three call sites in the qmd reference `sources_prepped` and currently expect
the post-filter list. Update them to `sources_filtered`:

- skeleton fill input (~line 409): `data_sources <- c(purrr::keep(sources_filtered, ~ nrow(.x) > 0L), list(database = database_reformat_prepped))`
- per-source join accounting (~line 422): `purrr::iwalk(sources_filtered, …)`
- completeness validation (~line 558): `all_prepped_keys <- dplyr::bind_rows(sources_filtered) %>% …`

`sources_prepped` (new meaning: post-`prep_for_skeleton`, pre-filter) is
kept around for diagnostic output in the manifest but is not used by the
skeleton join — the join consumes `sources_filtered`, matching today's
behaviour.

### 7. Update comments in `per_source_clean` (inline doc)

The block comment above `clean_one()` (lines ~169–186) should reflect the
new responsibility boundary — clean only, no prep, no filter. List the
chain as `key_rename → wnv_s_clean → make_key → culex_dedup` and add a
single line noting that `prep_for_skeleton()` and `filter_culex_sheet()` are
now in their own downstream chunks.

---

## Verification

Run end-to-end and inspect outputs:

1. Render the pipeline:
   ```sh
   quarto render pipelines/pipeline_combine_culex_sheet.qmd
   ```

2. Confirm Issue 2 is fixed — open `2_mid/culex_clean_all_spp_plot.png` and
   verify species other than Tarsalis and Pipiens appear in the fill legend
   (e.g., "Aedes vexans", "non culex", `NA`/blank for status events).

3. Confirm the all-spp CSV holds raw species values:
   ```sh
   cut -d, -f<spp-col> 3_output/culex_clean_all_spp.csv | sort -u
   ```
   Should show more than two unique values.

4. Confirm all six existing `stopifnot()` validations pass (counts printed
   to console for malfunction, no_mosquitoes, no_culex, completeness).

5. Diagnose Issue 1 from the new manifest. In `3_output/_manifest.txt`
   compare the row counts per source across stages:
   ```
   clean:<src>   n_after_dedup
   prep:<src>    n_out_prepped
   filter:<src>  n_out_filtered
   join:<src>    n_in_final_expand
   ```
   The drop point (dedup vs. prep vs. filter vs. skeleton join) for "no
   culex" trap-weeks should become apparent — that will tell us whether
   the next change is to `filter_culex_sheet`, `prep_for_skeleton`, or the
   skeleton fill priority.

6. Run unit tests:
   ```sh
   Rscript -e 'testthat::test_dir("tests/testthat")'
   ```
   No tests should break — `clean_one()` is private to the qmd, and the
   functions it now calls less of (`prep_for_skeleton`, `filter_culex_sheet`)
   already have their own tests that exercise them directly.

---

## Out of scope

- **Issue 1's actual fix** is deferred until the per-stage counts identify
  where "no culex" rows are dropped. This plan delivers the diagnostic
  surface, not the fix.
- No changes to `R/` functions, config YAML, or rename CSV.
- Downstream consumers (`join_cxsheet_database`, validations, plots that
  read `culex_filtered`) are unchanged.
