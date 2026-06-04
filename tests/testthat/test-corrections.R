test_that("correction scripts run only against their named election", {
  corrections_dir <- tempfile("corrections-")
  dir.create(corrections_dir)
  on.exit(unlink(corrections_dir, recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "dta <- dta |>",
      "  mutate(seat = seat + 1)"
    ),
    file.path(corrections_dir, "1.R")
  )

  dta <- dplyr::bind_rows(raw_fixture(id = 1), raw_fixture(id = 2))
  out <- cleaclean:::apply_corrections(
    dta,
    corrections_dir = corrections_dir,
    data_raw_dir = tempdir()
  )

  expect_equal(out$seat[out$id == 1], c(2, 1))
  expect_equal(out$seat[out$id == 2], c(1, 0))
  expect_equal(unique(out$id), c(1, 2))
})

test_that("the correction pipeline rejects an empty corrections directory", {
  corrections_dir <- tempfile("corrections-")
  dir.create(corrections_dir)
  on.exit(unlink(corrections_dir, recursive = TRUE), add = TRUE)

  expect_error(
    cleaclean:::apply_corrections(
      raw_fixture(),
      corrections_dir = corrections_dir,
      data_raw_dir = tempdir()
    ),
    "No election correction scripts"
  )
})
