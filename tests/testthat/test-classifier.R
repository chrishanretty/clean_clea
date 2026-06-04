test_that("the classifier applies V-Dem and manual rules", {
  vdem <- tibble::tibble(
    country_text_id = c("FIX", "ARG", "CIV"),
    year = c(2000, 2000, 2000),
    v2elloelsy = c(0, 7, 0),
    v2elloupdis = c(0, 0, 0),
    v2elthresh = c(0, 0, 0)
  )
  missing <- tibble::tibble(
    iso3 = "NEW",
    year = 2000,
    electoral_system = 0,
    threshold = 0
  )

  out <- cleaclean:::build_electoral_system_classifier(vdem, missing)

  expect_true(out$simple_system[out$iso3 == "FIX"])
  expect_false(out$simple_system[out$iso3 == "ARG"])
  expect_equal(
    as.character(out$electoral_system[out$iso3 == "CIV"]),
    "Block vote in multi-member districts"
  )
  expect_true(out$simple_system[out$iso3 == "NEW"])
})
