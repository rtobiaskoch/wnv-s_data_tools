test_that("pivot_boulder forces zone = BC when zone column already present", {
  raw <- tibble::tibble(
    trap_name        = "BC-12",
    date_trap_set    = "2022-06-13",
    mosquito_species = "Culex tarsalis",
    mosquito_count   = "8",
    trap_type        = "CDC Light Trap",
    zone             = "CO"  # wrong zone — should be forced to BC
  )
  out <- pivot_boulder(raw)
  expect_equal(unique(out$zone), "BC")
})

test_that("pivot_boulder adds zone = BC when zone column is absent", {
  raw <- tibble::tibble(
    trap_name        = "BC-12",
    date_trap_set    = "2022-06-13",
    mosquito_species = "Culex tarsalis",
    mosquito_count   = "8",
    trap_type        = "CDC Light Trap"
  )
  out <- pivot_boulder(raw)
  zcol <- if ("Zone" %in% names(out)) out$Zone else out$zone
  expect_equal(unique(zcol), "BC")
})

test_that("pivot_boulder reshapes wide Boulder sheets to long", {
  raw <- tibble::tibble(
    `Trap Number` = "BC-12",
    `Trap Date`   = "2022-06-13",
    `Cx tarsalis` = "8",
    `Cx pipiens`  = "0",
    `Zone`        = "BC"
  )
  out <- pivot_boulder(raw)
  expect_true("mosquito_species" %in% names(out))
  expect_equal(nrow(out), 2L)
})
