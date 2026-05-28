library(testthat)

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
  # zone2 derived the same way wnv_s_clean() does — required by prep_for_skeleton()
  zone2 <- if (zone %in% c("NE", "NW", "SE", "SW")) "FC" else zone
  data.frame(
    trap_id     = trap_id,
    zone        = zone,
    zone2       = zone2,
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
test_that("zone2 is preserved in output — required for downstream plotting", {
  result <- prep_for_skeleton(make_obs("Tarsalis", "culex", 10, zone = "NE"),
                              c("Tarsalis", "Pipiens"))
  expect_true("zone2" %in% names(result))
  expect_true(all(result$zone2 == "FC"))
})

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
