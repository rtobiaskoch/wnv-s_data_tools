library(testthat)

# Minimal trap reference: one active FC trap, one inactive BC trap
make_trap_ref <- function() {
  data.frame(
    trap_id = c("FC-001", "BC-001"),
    zone    = c("NE",     "BC"),
    active  = c(1L,       0L),
    start   = c(2020L,    2021L),
    stringsAsFactors = FALSE
  )
}

test_that("expand_trap returns required column names", {
  result <- expand_trap(
    make_trap_ref(), c("Tarsalis", "Pipiens"),
    year_start = 2021, year_end = 2021, week_start = 23L, week_end = 23L
  )
  expect_true(all(
    c("key", "trap_id", "zone", "zone2", "method", "year", "week", "spp")
    %in% names(result)
  ))
})

test_that("BC traps are included even when active == 0", {
  result <- expand_trap(
    make_trap_ref(), c("Tarsalis"),
    year_start = 2021, year_end = 2021, week_start = 23L, week_end = 23L
  )
  expect_true("BC-001" %in% result$trap_id)
})

test_that("inactive non-BC traps are excluded", {
  trap_ref <- data.frame(
    trap_id = "FC-099", zone = "NW", active = 0L, start = 2020L,
    stringsAsFactors = FALSE
  )
  result <- expand_trap(
    trap_ref, c("Tarsalis"),
    year_start = 2020, year_end = 2020, week_start = 23L, week_end = 23L
  )
  expect_equal(nrow(result), 0L)
})

test_that("zone2 is 'FC' for NE/NW/SE/SW and equals zone for others", {
  result <- expand_trap(
    make_trap_ref(), c("Tarsalis"),
    year_start = 2021, year_end = 2021, week_start = 23L, week_end = 23L
  )
  expect_equal(result$zone2[result$trap_id == "FC-001"], "FC")
  expect_equal(result$zone2[result$trap_id == "BC-001"], "BC")
})

test_that("method is 'G' for trap_ids containing 'gr', 'L' otherwise", {
  trap_ref <- data.frame(
    trap_id = c("FC-001gr", "FC-002"),
    zone    = c("NE", "NE"),
    active  = c(1L, 1L),
    start   = c(2020L, 2020L),
    stringsAsFactors = FALSE
  )
  result <- expand_trap(
    trap_ref, c("Tarsalis"),
    year_start = 2020, year_end = 2020, week_start = 23L, week_end = 23L
  )
  expect_equal(result$method[result$trap_id == "FC-001gr"], "G")
  expect_equal(result$method[result$trap_id == "FC-002"],   "L")
})

test_that("row count equals n_traps × n_years × n_weeks × n_spp", {
  # FC-001 active from 2020, BC-001 inactive from 2021 (included via BC rule)
  # year range 2021-2022 → 2 years; week range 23-25 → 3 weeks; 2 spp
  # FC-001: start=2020, both 2021 and 2022 qualify → 2 × 3 × 2 = 12
  # BC-001: start=2021 → 2 × 3 × 2 = 12
  result <- expand_trap(
    make_trap_ref(), c("Tarsalis", "Pipiens"),
    year_start = 2021, year_end = 2022, week_start = 23L, week_end = 25L
  )
  expect_equal(nrow(result), 24L)
})

test_that("traps started after year_end do not appear", {
  trap_ref <- data.frame(
    trap_id = "FC-001", zone = "NE", active = 1L, start = 2025L,
    stringsAsFactors = FALSE
  )
  result <- expand_trap(
    trap_ref, c("Tarsalis"),
    year_start = 2020, year_end = 2023, week_start = 23L, week_end = 23L
  )
  expect_equal(nrow(result), 0L)
})
