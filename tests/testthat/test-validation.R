test_that("a valid simple-system fixture passes all invariants", {
  result <- cleaclean:::validate_simple_systems(simple_fixture())
  expect_true(all(result$failures == 0))
})

test_that("validation reports an invalid seat total", {
  dta <- simple_fixture()
  dta$s <- c(0, 0)
  dta$ps <- c(0, 0)

  result <- cleaclean:::validate_simple_systems(dta)
  seat_total <- result[result$test == "sum(s) == m", ]

  expect_equal(unname(seat_total$failures), 1)
})

test_that("source loading rejects absent and mismatched raw data", {
  expect_error(
    cleaclean:::load_clea_source("not-a-real-file.RData", "abc"),
    "missing"
  )

  path <- tempfile(fileext = ".RData")
  object <- 1
  save(object, file = path)

  expect_error(cleaclean:::load_clea_source(path, "abc"), "checksum")
})

test_that("UTF-8 normalization preserves values and data-frame classes", {
  dta <- tibble::tibble(
    character = enc2native("Gr\u00fcne"),
    factor = factor(enc2native("Gr\u00fcne")),
    number = 1
  )

  result <- cleaclean:::normalize_utf8(dta)

  expect_identical(as.character(result$character), "Gr\u00fcne")
  expect_identical(as.character(result$factor), "Gr\u00fcne")
  expect_identical(result$number, dta$number)
  expect_s3_class(result, "tbl_df")
  expect_identical(Encoding(result$character), "UTF-8")
  expect_identical(Encoding(levels(result$factor)), "UTF-8")
})
