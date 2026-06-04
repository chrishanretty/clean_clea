# Rebuild the cleaclean package datasets ----------------------------------
#
# This is the main maintainer entrypoint for the entire data-cleaning
# pipeline. Run it from the package root with:
#
#   Rscript data-raw/build.R
#
# The script deliberately does not download the raw CLEA archive. Maintainers
# must obtain the source directly from CLEA and place it at the path specified
# below. The file is ignored by Git and is never included in the package.


# Locate the package and its maintainer data -------------------------------

# Resolve the current working directory to an absolute path. The build must be
# run from the package root, so this path is also the package root.
root <- normalizePath(".", mustWork = TRUE)

# Store the absolute path to all maintainer-only data and scripts. Passing this
# path explicitly means that correction scripts never depend on `here()` or on
# an RStudio project being open.
data_raw_dir <- file.path(root, "data-raw")

# Identify the locally downloaded raw CLEA lower-chamber dataset. This path is
# covered by `.gitignore`, so the source archive cannot be committed by
# accident.
raw_path <- file.path(
  data_raw_dir,
  "raw",
  "clea",
  "clea_lc_20251015.RData"
)

# Pin the exact raw source used for package version 0.1.0. The build stops
# before loading the file if its SHA-256 checksum differs from this value.
expected_sha256 <-
  "a63e075a41379bb8e1cb41664f9260b166d90fb5339c83a11d7f5f4382e96b87"


# Check the maintainer build environment ----------------------------------

# List every package required to rebuild the datasets. These are checked
# explicitly so that a missing dependency produces one clear error before the
# expensive data work begins.
required_packages <- c(
  "countrycode", # Convert CLEA country codes to ISO3 codes.
  "digest",      # Verify the raw CLEA file's SHA-256 checksum.
  "dplyr",       # Perform the main data transformations and corrections.
  "pkgload",     # Load unexported package functions during development.
  "readr",       # Read the tracked, manually assembled CSV inputs.
  "stringr",     # Support string operations used by correction scripts.
  "tibble",      # Preserve tibble classes throughout the build.
  "tidyr",       # Reshape manual correction data where required.
  "vdemdata"     # Supply the source electoral-system classifications.
)

# Test whether each required package is installed without attaching it to the
# search path. The result is a character vector containing only missing
# packages.
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

# Stop immediately and report every missing package in one message. This avoids
# failing part-way through a rebuild after the raw data have been processed.
if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before rebuilding: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# Load the current package source, including its unexported maintainer
# functions. The installed package intentionally exports datasets only, but
# this build needs access to the internal cleaning and validation functions.
pkgload::load_all(root, helpers = FALSE, export_all = TRUE, quiet = TRUE)


# Build the cleaned CLEA archive ------------------------------------------

# Verify the raw file checksum, load the expected `clea_lc_20251015` object
# into an isolated environment, and return it as the starting data frame.
clean_clea <- load_clea_source(raw_path, expected_sha256)

# Apply all election-specific scripts under `data-raw/corrections/`. Each
# script receives only the election rows it is responsible for, along with the
# explicit path to tracked manual inputs. The corrected elections are then
# reassembled in their original order.
clean_clea <- apply_corrections(
  clean_clea,
  corrections_dir = file.path(data_raw_dir, "corrections"),
  data_raw_dir = data_raw_dir
)

# Mark every non-ASCII string with its known UTF-8 encoding. This does not
# change any text values, but it ensures that R can interpret international
# party and candidate names consistently on every supported platform.
clean_clea <- normalize_utf8(clean_clea)

# Confirm that the corrected archive is a non-empty tibble with the expected
# public schema before using it to derive the simple-systems dataset.
validate_clean_clea(clean_clea)


# Build the electoral-system classifier ----------------------------------

# Read the tracked manual classifications that fill gaps and correct errors in
# the V-Dem electoral-system data.
missing_electoral_systems <- readr::read_csv(
  file.path(data_raw_dir, "manual", "missing_electoral_systems.csv"),
  show_col_types = FALSE
)

# Read the tracked United Nations geographic mapping used to add country,
# subregion, and region names to the analysis-ready dataset.
un_geoscheme <- readr::read_csv(
  file.path(data_raw_dir, "manual", "un_geoscheme.csv"),
  show_col_types = FALSE
)

# Combine V-Dem with the manual classifications and apply the documented rules
# that distinguish simple electoral systems from complex systems.
classifier <- build_electoral_system_classifier(
  vdemdata::vdem,
  missing_electoral_systems
)


# Build and validate the simple-systems dataset ---------------------------

# Transform the corrected CLEA archive into the analysis-ready simple-systems
# dataset, merge classifications and geography, and compute district-level
# electoral statistics.
simple_systems <- build_simple_systems(clean_clea, classifier, un_geoscheme)

# Apply the same explicit UTF-8 marking to the derived dataset so both public
# package datasets use a consistent, portable string encoding.
simple_systems <- normalize_utf8(simple_systems)

# Run all domain invariants before writing either dataset. A failed invariant
# stops the build and leaves the existing package data files untouched.
assert_valid_simple_systems(simple_systems)


# Replace the package datasets --------------------------------------------

# Save both validated objects into `data/` as xz-compressed `.rda` files.
# `save_package_data()` writes temporary files first and only replaces the
# existing package data after both saves succeed.
paths <- save_package_data(
  clean_clea,
  simple_systems,
  data_dir = file.path(root, "data")
)

# Print the exact output paths so maintainers can confirm what the build
# replaced.
message(
  "Rebuilt package datasets:\n",
  paste0("- ", paths, collapse = "\n")
)
