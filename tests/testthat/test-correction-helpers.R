test_that("FPTP seats are imputed from the largest vote", {
  dta <- raw_fixture(seat = c(-990, -990))
  out <- cleaclean:::impute_fptp_seats(dta, id = 1)

  expect_equal(out$seat, c(1, 0))
})

test_that("Netherlands elections collapse to one nationwide district", {
  dta <- raw_fixture(
    cst = c(1, 1, 2, 2),
    mag = c(1, 1, 1, 1),
    pty = c(1, 2, 1, 2),
    pv1 = c(60, 40, 55, 45),
    seat = c(1, 0, 1, 0)
  )
  out <- cleaclean:::collapse_netherlands(dta, id = 1)

  expect_equal(unique(out$cst), 901)
  expect_equal(unique(out$mag), 2)
  expect_equal(out$pv1[match(1, out$pty)], 115)
  expect_equal(out$seat[match(1, out$pty)], 2)
})

test_that("Cyprus rows collapse to party totals", {
  dta <- raw_fixture(pty = c(1, 1), pv1 = c(10, 10), seat = c(1, 0))
  out <- cleaclean:::collapse_cyprus(dta, id = 1)

  expect_equal(nrow(out), 1)
  expect_equal(out$seat, 1)
})

test_that("New Zealand list rows collapse and receive supplied seats", {
  dta <- raw_fixture(
    cst = c(901, 901, 902, 902),
    mag = 4,
    pty = c(1, 2, 1, 2),
    pv1 = c(60, 40, 50, 50),
    seat = 0
  )
  out <- cleaclean:::collapse_nz_list(
    dta,
    id = 1,
    mag = 4,
    seat_allocations = c("1" = 3, "2" = 1)
  )

  expect_equal(unique(out$cst), 900)
  expect_equal(out$pv1[match(1, out$pty)], 110)
  expect_equal(out$seat[match(1, out$pty)], 3)
})
