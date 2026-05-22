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

test_that("inventory_week_folders flags week folders missing a matched file", {
  tmp <- tempfile()
  dir.create(file.path(tmp, "WNV-s 2020 (X)", "Week 24", "Data from VDCI"),
             recursive = TRUE)
  dir.create(file.path(tmp, "WNV-s 2020 (X)", "Week 25"), recursive = TRUE)
  file.create(file.path(tmp, "WNV-s 2020 (X)", "Week 24", "Data from VDCI",
                        "LC Week24_2020_Culex.csv"))
  result <- inventory_week_folders(
    root    = tmp,
    pattern = "LC Week.*Culex\\.csv$"
  )
  expect_equal(result$n_year_folders, 1L)
  expect_equal(result$n_week_folders, 2L)
  expect_equal(result$n_matched, 1L)
  expect_equal(length(result$missing_folders), 1L)
  expect_true(grepl("Week 25", result$missing_folders))
})
