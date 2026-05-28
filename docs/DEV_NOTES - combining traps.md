# WNV-S Trap Combiner — Developer Notes

---

## Per-Source Import Architecture

### Why it exists

The original pipeline read a single pre-consolidated file
(`1_input/all_mosq/LC * all mosquitoes.csv`) produced by an upstream manual
step. The per-source architecture replaces that with a config-driven loader
that reads raw files from each data-provider's folder directly, so the
pipeline is reproducible from raw inputs and adding a new source is a
single YAML entry.

---

### The four moving parts

| Layer | Where | What it does |
|-------|-------|--------------|
| **Config registry** | `config/config_culex_combine.yml` | Declares each source: path, file-match pattern, pivot function name, filter flag, import args |
| **`read_source()`** | `R/fun_read_source.R` | Lists files, drops excluded paths, reads each with `rio::import`, optionally calls a pivot function, coerces everything to character, binds rows |
| **Pivot functions** | `R/fun_pivot_cmc.R`, `R/fun_pivot_cfc.R`, `R/fun_pivot_boulder.R` | Per-source reshape: wide→long for CMC/CFC, pass-through + zone enforcement for Boulder |
| **`clean_one()`** | `pipelines/pipeline_combine_culex_sheet.qmd` chunk `per_source_clean` | Runs the canonical chain on one source's raw tibble |

---

### Step-by-step data flow

```
config/config_culex_combine.yml
  └─ sources:
       vdci / cmc_weekly / cfc / bc
              │
              ▼
     [ per_source_import chunk ]
              │
              │  purrr::imap(cfg$sources, …)
              │
              ▼
        read_source(path, pattern, pivot_fn, exclude_paths, import_args)
              │
              ├─ list.files(recursive, ignore.case)
              ├─ filter out exclude_paths substrings
              ├─ for each file:
              │     rio::import(f, ...import_args)   ← extra args e.g. sheet="CombinedData"
              │     pivot_fn(raw)                    ← reshape if needed (NULL = skip)
              │     mutate(across(everything(), as.character))
              └─ bind_rows → sources_raw[[src_name]]
              │
              ▼
     [ per_source_clean chunk ]
              │
              │  purrr::imap(sources_raw, clean_one)
              │
              ▼
        clean_one(df_raw, src, src_name)
              │
              ├─ key_rename(keychain, drop_extra = TRUE)
              │     maps source column names → canonical names (trap_id, trap_date, …)
              ├─ wnv_s_clean()
              │     parses dates, derives zone2/year/week/method, classifies spp, sets trap_status
              ├─ make_key(c("trap_id","spp","year","week"))
              ├─ culex_dedup()
              │     removes non-standard collection nights (weekends)
              ├─ prep_for_skeleton(cfg$spp_levels)
                    expands each trap-date to one row per spp_level;
      
              │
              ▼
        sources_prepped[[src_name]]   ← named list, one prepped tibble per source
              │
              ▼
     [ join_skeleton chunk ]
              │
              │  data_sources = c(sources_prepped, list(database = database_reformat_prepped))
              │
              ▼
        fill_skeleton(trap_skeleton, data_sources)
              │
              │  priority order: vdci > cmc_weekly > cfc > bc > database
              │  each source fills only skeleton cells still NA after higher-priority sources
              └─ → culex_database_expand (43 260 rows matching the skeleton)
```

---

### The pivot functions

Each source has a different raw file layout:

| Source | Raw format | Pivot function | What it does |
|--------|-----------|---------------|--------------|
| **VDCI** (2019–present) | Long: one row per (trap, date, species) | *none* (`pivot_fn: null`) | Already in canonical long format |
| **CMC weekly** (2015–2018) | Wide: one row per trap, `Cx tarsalis` + `Cx pipiens` as separate columns | `pivot_cmc()` | Wide→long pivot; synthesizes a `"malfunction"` species row when `Malfunction != "NO"` so `wnv_s_clean()` can detect it; normalises `mosquito_count` to character before binding |
| **CFC 2006–2017** | Already long (one row per trap/species), but in `CombinedData` sheet of a multi-sheet xlsx | `pivot_cfc()` | No pivot — just renames CFC-specific columns (`Collection Site`, `Date`, `MMWR Week`) to the VDCI intermediate names that `key_rename()` already knows (`trap_name`, `date_trap_set`, `Week`) |
| **Boulder** (2021–present) | Long: same VDCI format | `pivot_boulder()` | Pass-through; forces `zone = "BC"` on every row (Boulder files sometimes omit the zone column) |

The pivot functions produce **intermediate column names** (e.g. `trap_name`, `date_trap_set`, `mosquito_species`, `mosquito_count`) — not the final canonical names. `key_rename()` handles the final translation using `1_input/database_column_rename.csv`.

---

### Config keys reference

```yaml
sources:
  <source_name>:
    path:           # root folder searched recursively by read_source()
    pattern:        # regex matched against full file paths (ignore.case = TRUE)
    pivot_fn:       # string name of function in pivot_registry, or null
    filter_active:  # true = apply filter_culex_sheet(); false = skip (Boulder)
    exclude_paths:  # list of substrings; files whose paths contain any are dropped
    import_args:    # optional named list forwarded to rio::import() (e.g. sheet:)
```

`pivot_fn` is stored as a string in YAML and resolved to the actual R function
via the `pivot_registry` list in the `per_source_import` chunk. Adding a new
source with a new format requires: (1) a new `R/fun_pivot_<name>.R`, (2) an
entry in `pivot_registry`, (3) a new block under `sources:` in the config.

---

### The `fill_skeleton` priority system

`fill_skeleton()` (`R/fun_fill_skeleton.R`) iterates the `data_sources` list in
order. For each source it left-joins only the skeleton rows that still have
`NA` in `trap_status` — so the first source to supply a record for a given
`(trap_id, spp, year, week)` key wins and later sources cannot overwrite it.

Consequence for the manifest's **balance check**: CFC rows that were superseded
by VDCI or CMC show `source = "vdci"` (not `"cfc"`) in the final expand, so
`n_in_final_expand` for CFC is lower than `n_into_skeleton`. This shows as
`DRIFT` in the manifest — it is **expected behaviour**, not data loss. The
completeness `stopifnot()` (validation #6) confirms that all CFC keys appear in
either `culex_database_expand` or `unmerged`, regardless of which source label
they carry.

---

### Validation chain (6 stopifnots)

| # | Check | Baseline |
|---|-------|---------|
| 1 | Malfunction trap-week count unchanged | `culex_filtered` semi-joined to skeleton |
| 2 | "No mosquitoes" count ≥ input | `culex_filtered` semi-joined to skeleton |
| 3 | "No culex" count unchanged | `culex_filtered` semi-joined to skeleton |
| 4 | malfunction + no trap rows have `total = NA` | `culex_database_expand` |
| 5 | no culex + no mosquitoes rows have `total = 0` | `culex_database_expand` |
| 6 | All prepped Tarsalis/Pipiens keys appear in expand or unmerged | combined `sources_prepped` |

Validations 1–3 restrict the baseline to skeleton-eligible keys
(`semi_join(trap_skeleton, by = "key")`) so historical records from CMC/CFC that
fall outside the skeleton's year/week scope (23–37) or outside the active-trap
reference don't inflate the baseline and cause false failures.

---

### Adding a new source (checklist)

1. Create `R/fun_pivot_<name>.R` if the raw format needs reshaping.
2. Add an entry to `pivot_registry` in the `per_source_import` chunk.
3. Add a block under `sources:` in `config/config_culex_combine.yml`.
4. Re-run `quarto render pipelines/pipeline_combine_culex_sheet.qmd`.
5. Check the manifest (`3_output/_manifest.txt`) — all join `balance_check`
   lines should end in `OK` or have an explainable `DRIFT`, and `missing keys`
   in validation #6 should be 0.
