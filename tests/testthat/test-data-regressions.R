data("clean_clea", package = "cleaclean")

test_that("known unusable elections remain excluded", {
  expect_false(any(clean_clea$id %in% c(7, 1169)))
})

test_that("representative direct corrections remain applied", {
  loeak <- clean_clea[
    clean_clea$id == 1252 & clean_clea$can == "CHRISTOPHER J LOEAK",
  ]

  expect_equal(nrow(loeak), 1)
  expect_equal(loeak$seat, 1)
})

test_that("manual Swedish patches remain applied", {
  sweden <- clean_clea[clean_clea$id %in% c(584, 585), ]

  expect_equal(nrow(sweden), 359)
  expect_true(all(sweden$mag > 0))
  expect_true(all(sweden$seat >= 0 | is.na(sweden$seat)))
  expect_equal(
    as.numeric(tapply(sweden$seat, sweden$id, sum, na.rm = TRUE)),
    c(230, 222)
  )
})

test_that("representative structural corrections remain applied", {
  netherlands <- clean_clea[clean_clea$id == 498, ]
  jamaica <- clean_clea[clean_clea$id == 423, ]
  cyprus <- clean_clea[clean_clea$id == 1795, ]
  new_zealand <- clean_clea[clean_clea$id == 1057 & clean_clea$cst == 900, ]

  expect_equal(unique(netherlands$cst), 901)
  expect_equal(sum(jamaica$seat), 53)
  expect_false(any(duplicated(cyprus[c("cst", "pty")])))
  expect_equal(sum(new_zealand$seat), 53)
})
