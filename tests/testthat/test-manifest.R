test_that("manifest_init creates file with header", {
  tmp <- tempfile(fileext = ".txt")
  manifest_init(tmp)
  txt <- readLines(tmp)
  expect_true(any(grepl("MANIFEST", txt)))
  expect_true(any(grepl("Generated:", txt)))
})

test_that("manifest_log appends without truncating", {
  tmp <- tempfile(fileext = ".txt")
  manifest_init(tmp)
  manifest_log(tmp, "vdci read", n_files = 41, n_rows = 12345)
  manifest_log(tmp, "vdci clean", n_rows = 12300)
  txt <- readLines(tmp)
  expect_true(any(grepl("vdci read", txt)))
  expect_true(any(grepl("vdci clean", txt)))
  expect_true(any(grepl("12345", txt)))
})

test_that("inventory_source counts files matching pattern", {
  tmp <- tempfile()
  dir.create(file.path(tmp), recursive = TRUE)
  file.create(file.path(tmp, "LC Week24_2020_Culex.csv"))
  file.create(file.path(tmp, "LC Week25_2020_Culex.csv"))
  file.create(file.path(tmp, "readme.txt"))
  result <- inventory_source(tmp, pattern = "\\.csv$")
  expect_equal(result$n_files, 2L)
  expect_true(all(grepl("\\.csv$", result$files)))
})
