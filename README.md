# wnv-s_trap_hx_combiner

Compile the WNV-S culex trap-level dataset used for downstream historical
abundance calculations (average mosquitoes per trap per night, for *Culex
tarsalis* and *Culex pipiens*). Combines four heterogeneous trap-level
sources from 2006–present, supplements gaps with pooled-testing records,
and joins everything into a single trap × week × year × species skeleton
so that "trap not set" is distinguishable from "trap set but caught zero."

---

## How to run

```r
# from the project root in R / RStudio
quarto::quarto_render("pipelines/pipeline_combine_culex_sheet.qmd")
```

or from the shell:

```sh
quarto render pipelines/pipeline_combine_culex_sheet.qmd
```

That single command runs the full pipeline, writes every intermediate and
final artifact, and produces `pipelines/pipeline_combine_culex_sheet.html`
as a human-readable render log. After it finishes:

- `3_output/_manifest.txt` — per-stage row counts, balance checks, and
  final validation totals. **Read this first if anything looks off.**
- `3_output/culex_sheet_database_expand.csv` — the canonical filled skeleton
  (one row per active-trap × week × year × spp_level).
- `3_output/culex_clean_all_spp.csv` — all observed species (including
  non-culex), pre-filter, for diagnostics.
- `2_mid/*` and `3_output/*.png` — intermediate CSVs and plots.

### Configuration

All pipeline behaviour is driven by `config/config_culex_combine.yml`:

- `year_start`, `week_start`, `week_end` — skeleton bounds (default
  2006-, weeks 23–37).
- `spp_levels` — species the skeleton expands to (`Tarsalis`, `Pipiens`).
- `fn_trap_keep` — active-trap reference (`foco_trap - data.csv`).
- `fn_database` — pooled-testing supplement (`wnv-s_database - data.csv`).
- `sources:` — registry of trap-level data providers; see the per-source
  section below.

### Tests

```sh
Rscript -e 'testthat::test_dir("tests/testthat")'
```

---

## Major pipeline stages

The qmd is organised top-to-bottom; each section below names the chunk
and the R functions it calls.

### 1. Per-source import — chunk `per_source_import`

For each entry in `cfg$sources`, `read_source()` (`R/fun_read_source.R`):
lists files matching `pattern`, drops paths in `exclude_paths`, reads each
file via `rio::import()` (with optional `import_args` like
`sheet = "CombinedData"`), calls a source-specific `pivot_fn` if set, and
coerces every column to character before binding rows.

Pivot functions reshape each provider's idiosyncratic raw format into a
common VDCI-compatible long format (`trap_name`, `date_trap_set`,
`mosquito_species`, `mosquito_count`, …):

| Source | Years | Format | Pivot |
|---|---|---|---|
| VDCI | 2019–present | already long | none |
| CMC weekly | 2015–2018 | wide, one col per species | `pivot_cmc()` |
| CFC | 2006–2017 | long but Excel-serial dates | `pivot_cfc()` |
| Boulder County | 2021–present | long, no zone | `pivot_boulder()` |

### 2. Per-source clean — chunk `per_source_clean`

`clean_one()` runs the canonical cleaning chain on each source:
`key_rename()` (translates source columns to canonical names using
`1_input/database_column_rename.csv`) → `wnv_s_clean()` (parses dates,
derives `zone`/`zone2`/`year`/`week`/`method`, classifies species, assigns
`trap_status`) → `make_key()` → `culex_dedup()` (removes weekend
collection nights and exact duplicates).

This stage **does not drop rows for analysis reasons** — it cleans only.
All observed species and traps survive, so the all-species diagnostic
plot (`2_mid/culex_clean_all_spp_plot.png`) shows the full picture.

### 3. Per-source prep — chunk `per_source_prep`

`prep_for_skeleton()` (`R/fun_prep_for_skeleton.R`) expands each cleaned
trap-date to one row per `spp_level`, sets `total = NA` for `malfunction`
events and `total = 0` for `no culex` / `no mosquitoes` events, and
restricts to species in `cfg$spp_levels`. This is the first stage where
non-target species drop out.

### 4. Per-source filter — chunk `per_source_filter`

`filter_culex_sheet()` (`R/fun_filter_culex_sheet.R`) applies the
active-trap semi-join against `foco_trap - data.csv` for each source
whose config has `filter_active: true`. Boulder bypasses this filter
because its trap set is sporadic and not in the active reference.

### 5. Skeleton build and fill — chunks "EXPAND GRID" and `join_skeleton`

`expand_trap()` (`R/fun_expand_trap.R`) builds the complete
trap × week × year × spp skeleton from `foco_trap - data.csv`. Every
active trap × every week in 23–37 × every year from `year_start`
forward × `Tarsalis`/`Pipiens` is a row.

`fill_skeleton()` (`R/fun_fill_skeleton.R`) iterates `data_sources` in
priority order — `vdci > cmc_weekly > cfc > bc > database` — and fills
only the cells still NA after higher-priority sources have run. This is
how the priority semantics are enforced.

### 6. Database supplement

`reformat_database_2_culex_sheet()` reshapes `wnv-s_database - data.csv`
(pooled-testing records) into the same intermediate format and runs it
through the same clean/prep chain. It enters `fill_skeleton()` last and
patches any cells the culex-sheet sources couldn't supply (e.g. CDC
Berthoud traps in 2024 before VDCI took the contract).

### 7. Post-expand status and validation — chunk `post_expand_status`

`assign_trap_status_post_expand()` (`R/fun_assign_trap_status.R`) labels
remaining-NA skeleton rows (active trap × week with no source coverage).
Six `stopifnot()` validations then check:

1–3. Malfunction / no_mosquitoes / no_culex trap-week counts match input
   (after restricting input to skeleton-eligible keys).
4. `malfunction` and `no trap` rows have `total = NA`.
5. `no culex` and `no mosquitoes` rows have `total = 0`.
6. Every Tarsalis/Pipiens key from any source lands in either
   `culex_database_expand` or `unmerged` — no silent drops.

### 8. Outputs

Written by the last few chunks:

- `3_output/culex_sheet_database_expand.csv` — final filled skeleton.
- `3_output/culex_clean_all_spp.csv` — all observed species before prep.
- `2_mid/unmerged_data.csv` — records that didn't match the skeleton
  (out-of-season, inactive traps).
- `3_output/trap_status.png`, `3_output/culex_v_datasheet.png`,
  `2_mid/culex_clean_all_spp_plot.png` — diagnostic plots.
- `3_output/_manifest.txt` — per-source per-stage row counts and balance
  checks.

---

## Repo layout

```
1_input/                        raw data (per-source folders)
  culex_sheet/                  VDCI/CMC/CFC weekly trap data
  foco_trap - data.csv          active-trap reference
  wnv-s_database - data.csv     pooled-testing supplement
  database_column_rename.csv    column-name translation table
2_mid/                          intermediate CSVs + plots
3_output/                       final CSVs + plots + manifest
R/                              source functions (one per file)
config/                         YAML pipeline configuration
pipelines/                      qmd pipeline scripts
tests/testthat/                 unit tests
docs/                           DEV_NOTES.md, TODO.md, plans
```

For deeper architecture detail, see `docs/DEV_NOTES.md`.
