test_that("pivot_cmc emits one row per (trap, species)", {
  raw <- tibble::tibble(
    `Trap Number`    = c("FC-029gr", "LV-020"),
    `Trap Date`      = c("06/16/2016", "06/15/2016"),
    `Zone`           = c("SE", "LV"),
    `Light / Gravid` = c("GRAVID", "LIGHT"),
    `Malfunction`    = c("NO", "NO"),
    `Cx tarsalis`    = c("0", "17"),
    `Cx pipiens`     = c("25", "0"),
    `Total CX`       = c("25", "17"),
    `Week`           = c("24", "24")
  )
  out <- pivot_cmc(raw)
  expect_equal(nrow(out), 4L)
  expect_setequal(out$mosquito_species, c("Cx tarsalis", "Cx pipiens"))
  expect_true(all(c("trap_name", "mosquito_count", "date_trap_set", "Zone") %in% names(out)))
})

test_that("pivot_cmc emits malfunction row when Malfunction != NO", {
  raw <- tibble::tibble(
    `Trap Number`    = "FC-001",
    `Trap Date`      = "06/16/2016",
    `Zone`           = "SE",
    `Light / Gravid` = "LIGHT",
    `Malfunction`    = "YES",
    `Cx tarsalis`    = NA,
    `Cx pipiens`     = NA,
    `Total CX`       = NA,
    `Week`           = "24"
  )
  out <- pivot_cmc(raw)
  expect_true(any(stringr::str_detect(out$mosquito_species, "(?i)malfunction")))
})
