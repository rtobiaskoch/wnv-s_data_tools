library(testthat)
source(here::here("R/fun_assign_trap_status.R"))

# Post-join skeleton: FC-001 matched (culex), FC-002/FC-003 unmatched in active
# zone, LV-001 unmatched in a zone with NO data at all
make_post_join <- function() {
  data.frame(
    trap_id     = c("FC-001", "FC-002", "FC-003", "LV-001"),
    zone        = c("NE",     "NE",     "NE",     "LV"),
    year        = c(2023L,    2023L,    2023L,    2023L),
    week        = c(25L,      25L,      25L,      25L),
    spp         = c("Tarsalis","Tarsalis","Tarsalis","Tarsalis"),
    trap_status = c("culex",  NA,        NA,        NA),
    total       = c(10,        NA,        NA,        NA),
    trap_date   = as.Date(c("2023-06-19", NA, NA, NA)),
    stringsAsFactors = FALSE
  )
}

test_that("existing trap_status is preserved", {
  result <- assign_trap_status_post_expand(make_post_join())
  expect_equal(result$trap_status[result$trap_id == "FC-001"], "culex")
  expect_equal(result$total[result$trap_id == "FC-001"], 10)
})

test_that("unmatched trap in zone-week WITH data gets 'no mosquitoes', total = 0", {
  result <- assign_trap_status_post_expand(make_post_join())
  fc002 <- result[result$trap_id == "FC-002", ]
  expect_equal(fc002$trap_status, "no mosquitoes")
  expect_equal(fc002$total, 0)
})

test_that("unmatched trap in zone-week with NO data gets 'no trap', total = NA", {
  result <- assign_trap_status_post_expand(make_post_join())
  lv001 <- result[result$trap_id == "LV-001", ]
  expect_equal(lv001$trap_status, "no trap")
  expect_true(is.na(lv001$total))
})

test_that("all unmatched traps in an active zone-week get 'no mosquitoes'", {
  result <- assign_trap_status_post_expand(make_post_join())
  ne_unmatched <- result[result$zone == "NE" & result$trap_id != "FC-001", ]
  expect_true(all(ne_unmatched$trap_status == "no mosquitoes"))
  expect_true(all(ne_unmatched$total == 0))
})

test_that("malfunction status is preserved — not overwritten by zone-week logic", {
  df <- data.frame(
    trap_id     = c("FC-001", "FC-002"),
    zone        = c("NE",     "NE"),
    year        = c(2023L,    2023L),
    week        = c(25L,      25L),
    spp         = c("Tarsalis","Tarsalis"),
    trap_status = c("malfunction", NA),
    total       = c(NA_real_,      NA_real_),
    trap_date   = as.Date(c("2023-06-19", NA)),
    stringsAsFactors = FALSE
  )
  result <- assign_trap_status_post_expand(df)
  expect_equal(result$trap_status[result$trap_id == "FC-001"], "malfunction")
  expect_true(is.na(result$total[result$trap_id == "FC-001"]))
  # FC-002 unmatched but zone has data (malfunction counts as data) -> no mosquitoes
  expect_equal(result$trap_status[result$trap_id == "FC-002"], "no mosquitoes")
})

test_that("no trap rows have NA total regardless of input total value", {
  df <- data.frame(
    trap_id = "LV-001", zone = "LV", year = 2023L, week = 25L,
    spp = "Tarsalis", trap_status = NA, total = 0,
    trap_date = as.Date(NA), stringsAsFactors = FALSE
  )
  result <- assign_trap_status_post_expand(df)
  expect_equal(result$trap_status, "no trap")
  expect_true(is.na(result$total))
})

test_that("'no mosquitoes' total is 0 even when input total was NA", {
  result <- assign_trap_status_post_expand(make_post_join())
  fc002 <- result[result$trap_id == "FC-002", ]
  expect_equal(fc002$total, 0)
  expect_false(is.na(fc002$total))
})

test_that("'no mosquitoes' total is forced to 0 even if source provided a non-NA value", {
  df <- data.frame(
    trap_id     = c("FC-001", "FC-002"),
    zone        = c("NE",     "NE"),
    year        = c(2023L,    2023L),
    week        = c(25L,      25L),
    spp         = c("Tarsalis","Tarsalis"),
    trap_status = c("culex",   "no mosquitoes"),
    total       = c(10,         5),   # "no mosquitoes" with erroneous non-zero total
    trap_date   = as.Date(c("2023-06-19", "2023-06-19")),
    stringsAsFactors = FALSE
  )
  result <- assign_trap_status_post_expand(df)
  expect_equal(result$total[result$trap_id == "FC-002"], 0)
})
