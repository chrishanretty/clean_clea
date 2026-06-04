test_that("source loading verifies and returns the expected isolated object", {
  skip_if_not_installed("digest")

  path <- tempfile(fileext = ".RData")
  clea_lc_20251015 <- raw_fixture()
  save(clea_lc_20251015, file = path)

  sha256 <- digest::digest(path, algo = "sha256", file = TRUE)
  out <- cleaclean:::load_clea_source(path, sha256)

  expect_identical(out, clea_lc_20251015)
})

test_that("package-data writing round trips both compressed objects", {
  data_dir <- tempfile("package-data-")
  on.exit(unlink(data_dir, recursive = TRUE), add = TRUE)

  clean_clea <- raw_fixture()
  simple_systems <- simple_fixture()
  paths <- cleaclean:::save_package_data(clean_clea, simple_systems, data_dir)

  clean_env <- new.env(parent = emptyenv())
  simple_env <- new.env(parent = emptyenv())
  load(paths[1], envir = clean_env)
  load(paths[2], envir = simple_env)

  expect_identical(clean_env$clean_clea, clean_clea)
  expect_identical(simple_env$simple_systems, simple_systems)
  expect_true(all(tools::checkRdaFiles(data_dir)$compress == "xz"))
})
