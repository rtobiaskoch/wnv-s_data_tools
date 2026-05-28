# Debug: Test Failures in 5 Test Files

## Context

Five test files were added in commit `c843a35` (refactor: per-source culex
ingestion) alongside five new source files they exercise. All tests are failing.
The goal is to identify the root cause(s) and make them pass without touching
test logic or source function logic (which is correct).

A note on naming: the user typed `test-pivot-fcf.R` — the actual file is
`tests/testthat/test-pivot_cfc.R` (underscore, "cfc" not "fcf"). This is
just a typo; no action needed.

---

## Root Cause: Missing `DESCRIPTION` and `NAMESPACE` files

### Evidence

| Observation | Implication |
|---|---|
| `find . -name DESCRIPTION` → nothing | Project is not an R package |
| `find . -name NAMESPACE` → nothing | Exports not declared |
| `.Rproj` has no `BuildType: Package` | RStudio not treating as package |
| `tests/testthat/` has no `helper.R` or `setup.R` | No fallback to source R/ files |
| All 5 tested functions carry `@export` in roxygen docs | They were written as package functions |
| `CLAUDE.md` says "Load with `devtools::load_all(".")`" | Package infrastructure is expected |

**Conclusion:** `devtools::load_all()` and `devtools::test()` both require at
minimum a `DESCRIPTION` file. Without it, none of the functions in `R/` are
loaded when tests run, so every test block fails immediately with:

```
Error: could not find function "manifest_init"
Error: could not find function "pivot_boulder"
... (same for pivot_cmc, pivot_cfc, read_source)
```

This is a **single root cause** that explains all five files failing
simultaneously.

---

## Fix Plan

### Step 1 — Confirm errors (read-only)

Run tests to capture the actual error messages before touching anything:

```r
# In R console (after devtools::load_all() fails, try directly):
testthat::test_dir("tests/testthat/")
```

Expected: "could not find function" for all five function names. This
confirms RC1 and that no secondary code-logic bugs exist.

### Step 2 — Create `DESCRIPTION`

Create a minimal `DESCRIPTION` at the project root. Package name must be
alphanumeric/dot only; use `trapHxCombiner`.

```
Package: trapHxCombiner
Type: Package
Title: WNV Surveillance Trap Historical Abundance Combiner
Version: 0.1.0
Authors@R: person("Robert", "Koch", email = "r.tobiaskoch@gmail.com",
    role = c("aut", "cre"))
Description: Combines trap-level mosquito surveillance data from VDCI, CMC,
    CFC, and Boulder County sources to compute historical abundance metrics
    for West Nile virus surveillance in northern Colorado.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Imports:
    cli,
    dplyr,
    lubridate,
    purrr,
    rlang,
    rio,
    stringr,
    tibble,
    tidyr,
    yaml
Suggests:
    devtools,
    testthat (>= 3.0.0)
Config/testthat/edition: 3
```

> **Why `Imports` not `Depends`?** `Imports` is the RSE-correct choice —
> it makes dependencies available to package functions without attaching them
> to the user's search path.

### Step 3 — Generate `NAMESPACE` via roxygen2

The R/ functions already have `@export` (and some `@importFrom`) tags.
Run once:

```r
devtools::document()
```

This generates `NAMESPACE` from the existing roxygen tags, making all
`@export` functions visible when the package is loaded.

> Do NOT hand-write NAMESPACE. `devtools::document()` is the single source
> of truth.

### Step 4 — Create `tests/testthat.R`

```r
library(testthat)
library(trapHxCombiner)

test_check("trapHxCombiner")
```

This is the standard runner file; without it `R CMD check` won't run tests.

### Step 5 — Run `devtools::test()` and triage any remaining failures

```r
devtools::test()
```

After RC1 is fixed, any remaining failures will be genuine code-logic bugs.
Expected: all 12 test blocks across the 5 files should pass based on code
review (logic in each source function matches its test assertions exactly —
see analysis below).

---

## Code-Logic Analysis (pre-confirmed correct)

A line-by-line review of each source function against its tests shows no
logic bugs assuming functions load. This is documented here so fixes in
Step 5 start with a narrow scope.

| File | Tests | Logic verdict |
|---|---|---|
| `fun_manifest.R` | 3 | ✅ `writeLines`/`cat(append=TRUE)` produces text `readLines` can grep |
| `fun_pivot_boulder.R` | 3 | ✅ wide→long branch and zone-force branch both correct |
| `fun_pivot_cmc.R` | 2 | ✅ `pivot_longer` + malfunction row synthesis correct |
| `fun_pivot_cfc.R` | 2 | ✅ rename-only (no pivot needed), numeric date guard correct |
| `fun_read_source.R` | 3 | ✅ exclude filter, empty-return, pivot_fn application all correct |

If any tests still fail after Steps 1–4, the likely suspects are:

- **`rio` not installed** — `read_source` calls `rio::import()`; run
  `install.packages("rio")` or `renv::install("rio")`.
- **`cli` not installed** — `clean_summary()` (called internally) uses `cli::cli_alert_*`.
- **`rlang` version mismatch** — `rlang::enquo` / `rlang::as_name` are stable
  since rlang 0.4.x; unlikely but check if `renv.lock` is stale.

---

## Files to Create / Modify

| File | Action |
|---|---|
| `DESCRIPTION` | **Create** (new — root cause fix) |
| `NAMESPACE` | **Generate** via `devtools::document()` |
| `tests/testthat.R` | **Create** (standard runner) |
| `R/*.R` files | No changes needed |
| `tests/testthat/test-*.R` | No changes needed |

---

## Verification

```r
# 1. Confirm package loads cleanly
devtools::load_all(".")         # should print "Loading trapHxCombiner"

# 2. Run all tests
devtools::test()                 # all 12 test blocks should pass

# 3. Optional: full package check
devtools::check(error_on = "warning")
```

Success criterion: `devtools::test()` reports 0 failures, 0 errors across
`test-manifest.R`, `test-pivot_boulder.R`, `test-pivot_cfc.R`,
`test-pivot_cmc.R`, `test-read_source.R`.
