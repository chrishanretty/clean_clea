test_that("effective-party and disproportionality metrics are correct", {
  expect_equal(cleaclean:::effective_parties(c(0.5, 0.5)), 2)
  expect_equal(cleaclean:::effective_parties(1), 1)
  expect_equal(cleaclean:::disproportionality(c(0.5, 0.5), c(1, 0)), 0.5)
})

test_that("wasted-vote bounds are vectorised and ordered", {
  lower <- cleaclean:::w_min(c(2, 2.5), c(3, 4), c(2, 2))
  upper <- cleaclean:::w_max(c(2, 2.5), c(3, 4), c(2, 2))

  expect_length(lower, 2)
  expect_length(upper, 2)
  expect_true(all(lower <= upper))
  expect_true(all(lower >= 0))
})

test_that("rank-size consistency detects inversions", {
  expect_true(cleaclean:::rank_size(c(0.6, 0.4), c(1, 0)))
  expect_false(cleaclean:::rank_size(c(0.6, 0.4), c(0, 1)))
})
