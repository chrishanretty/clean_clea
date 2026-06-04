data("clean_clea", package = "cleaclean")
data("simple_systems", package = "cleaclean")

test_that("clean_clea has the expected public schema", {
  expect_s3_class(clean_clea, "tbl_df")
  expect_equal(dim(clean_clea), c(1296813, 33))
  expect_equal(length(unique(clean_clea$id)), 2247)
  expect_identical(names(clean_clea), cleaclean:::clean_clea_columns())
})

test_that("simple_systems has the expected public schema", {
  expect_s3_class(simple_systems, "tbl_df")
  expect_equal(dim(simple_systems), c(280569, 31))
  expect_equal(length(unique(simple_systems$id)), 604)
  expect_equal(length(unique(paste(simple_systems$id, simple_systems$cst))), 46066)
  expect_equal(length(unique(simple_systems$iso3)), 111)
  expect_identical(names(simple_systems), cleaclean:::simple_systems_columns())
  expect_identical(
    levels(simple_systems$electoral_system),
    cleaclean:::electoral_system_levels()
  )
})

test_that("all published simple-system invariants pass", {
  result <- cleaclean:::validate_simple_systems(simple_systems)
  expect_true(all(result$failures == 0), info = paste(result$test, result$failures))
})
