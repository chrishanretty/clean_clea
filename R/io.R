load_clea_source <- function(path, expected_sha256) {
  if (!file.exists(path)) {
    stop(
      "The raw CLEA source file is missing: ",
      path,
      "\nDownload it manually and place it at this path before rebuilding.",
      call. = FALSE
    )
  }

  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required to verify the CLEA source.", call. = FALSE)
  }

  actual_sha256 <- digest::digest(path, algo = "sha256", file = TRUE)

  if (!identical(actual_sha256, expected_sha256)) {
    stop(
      "The raw CLEA source checksum does not match.\nExpected: ",
      expected_sha256,
      "\nActual:   ",
      actual_sha256,
      call. = FALSE
    )
  }

  source_env <- new.env(parent = emptyenv())
  loaded_names <- load(path, envir = source_env)

  if (!identical(loaded_names, "clea_lc_20251015")) {
    stop(
      "Expected the source file to contain only 'clea_lc_20251015'.",
      call. = FALSE
    )
  }

  source_env$clea_lc_20251015
}

normalize_utf8 <- function(dta) {
  dta[] <- lapply(dta, function(column) {
    if (is.character(column)) {
      return(enc2utf8(column))
    }

    if (is.factor(column)) {
      levels(column) <- enc2utf8(levels(column))
    }

    column
  })

  dta
}

save_package_data <- function(clean_clea, simple_systems, data_dir = "data") {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  clean_path <- file.path(data_dir, "clean_clea.rda")
  simple_path <- file.path(data_dir, "simple_systems.rda")
  clean_tmp <- tempfile("clean_clea_", tmpdir = data_dir, fileext = ".rda")
  simple_tmp <- tempfile("simple_systems_", tmpdir = data_dir, fileext = ".rda")

  on.exit(unlink(c(clean_tmp, simple_tmp)), add = TRUE)

  save(clean_clea, file = clean_tmp, compress = "xz", version = 2)
  save(simple_systems, file = simple_tmp, compress = "xz", version = 2)

  if (!file.rename(clean_tmp, clean_path)) {
    stop("Could not replace ", clean_path, ".", call. = FALSE)
  }

  if (!file.rename(simple_tmp, simple_path)) {
    stop("Could not replace ", simple_path, ".", call. = FALSE)
  }

  invisible(c(clean_path, simple_path))
}
