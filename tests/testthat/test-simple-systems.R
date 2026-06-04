test_that("the simple-systems pipeline derives an analysis-ready district", {
  skip_if_not_installed("countrycode")

  clean_clea <- raw_fixture()
  clean_clea$ctr <- 826

  classifier <- tibble::tibble(
    iso3 = "GBR",
    year = 2000,
    electoral_system = factor(
      "First-past-the-post in single-member districts",
      levels = cleaclean:::electoral_system_levels()
    ),
    simple_system = TRUE,
    threshold = FALSE
  )

  un_geoscheme <- tibble::tibble(
    iso3 = "GBR",
    m49 = 826,
    iso2 = "GB",
    country = "United Kingdom",
    subregion = "Northern Europe",
    region = "Europe"
  )

  out <- cleaclean:::build_simple_systems(
    clean_clea,
    classifier,
    un_geoscheme
  )

  expect_identical(names(out), cleaclean:::simple_systems_columns())
  expect_equal(out$v, c(10, 5))
  expect_equal(out$s, c(1, 0))
  expect_equal(out$pv, c(2 / 3, 1 / 3))
  expect_true(all(cleaclean:::validate_simple_systems(out)$failures == 0))
})
