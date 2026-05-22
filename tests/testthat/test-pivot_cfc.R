test_that("pivot_cfc renames CFC columns to VDCI intermediate names", {
  raw <- tibble::tibble(
    `Collection Site` = c("FC-029gr", "LV-020"),
    `Date`            = c("2010-06-16", "2010-06-15"),
    `MMWR Week`       = c("24", "24"),
    `Zone`            = c("SE", "LV"),
    `Species`         = c("pipiens", "tarsalis"),
    `Total`           = c("25", "17"),
    `Year`            = c("2010", "2010"),
    `Method`          = c("Gravid", "Light"),
    `ID`              = c("1", "2")
  )
  out <- pivot_cfc(raw)
  expect_true("trap_name" %in% names(out))
  expect_true("date_trap_set" %in% names(out))
  expect_true("Week" %in% names(out))
  expect_true("mosquito_species" %in% names(out))
  expect_true("mosquito_count" %in% names(out))
  expect_equal(nrow(out), 2L)
  expect_equal(out$trap_name, c("FC-029gr", "LV-020"))
})

test_that("pivot_cfc passes through rows without altering species counts", {
  raw <- tibble::tibble(
    `Collection Site` = "FC-053",
    `Date`            = "2006-06-07",
    `MMWR Week`       = "23",
    `Zone`            = "SE",
    `Species`         = "tarsalis",
    `Total`           = "5",
    `Year`            = "2006"
  )
  out <- pivot_cfc(raw)
  expect_equal(nrow(out), 1L)
  expect_equal(out$mosquito_count, "5")
})
