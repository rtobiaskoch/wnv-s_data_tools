test_that("read_source returns empty tibble when no files match", {
  result <- read_source(
    path          = tempdir(),
    pattern       = "nothing_matches\\.csv$",
    pivot_fn      = NULL,
    exclude_paths = character()
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("read_source excludes paths containing exclude tokens", {
  tmp <- tempfile()
  dir.create(file.path(tmp, "all_mosq"), recursive = TRUE)
  writeLines("a,b\n1,2", file.path(tmp, "all_mosq", "x.csv"))
  writeLines("a,b\n3,4", file.path(tmp, "x.csv"))
  result <- read_source(
    path = tmp, pattern = "\\.csv$", pivot_fn = NULL,
    exclude_paths = "all_mosq"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(as.character(result$a), "3")
})

test_that("read_source applies pivot_fn when provided", {
  tmp <- tempfile()
  dir.create(tmp, recursive = TRUE)
  writeLines("trap,tar,pip\nFC-001,3,5", file.path(tmp, "wide.csv"))
  pivot <- function(df) {
    tidyr::pivot_longer(df, c(tar, pip),
                        names_to  = "mosquito_species",
                        values_to = "mosquito_count")
  }
  result <- read_source(
    path = tmp, pattern = "\\.csv$", pivot_fn = pivot,
    exclude_paths = character()
  )
  expect_equal(nrow(result), 2L)
  expect_setequal(result$mosquito_species, c("tar", "pip"))
})
