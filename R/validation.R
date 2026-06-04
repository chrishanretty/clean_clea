clean_clea_columns <- function() {
  c(
    "release", "id", "rg", "ctr_n", "ctr", "yr", "mn", "sub", "cst_n", "cst",
    "mag", "pty_n", "pty", "can", "pev1", "vot1", "vv1", "ivv1", "to1", "cv1",
    "cvs1", "pv1", "pvs1", "pev2", "vot2", "vv2", "ivv2", "to2", "cv2",
    "cvs2", "pv2", "pvs2", "seat"
  )
}

simple_systems_columns <- function() {
  c(
    "id", "iso3", "country", "subregion", "region", "yr", "mn", "cst_n", "cst",
    "m", "pty_n", "pty", "can", "c", "p", "v", "s", "pv", "ps", "uncontested",
    "electoral_system", "threshold", "nv0", "ns0", "nv2", "ns2", "d", "w", "tx",
    "tr", "tmin"
  )
}

validate_clean_clea <- function(dta) {
  if (!inherits(dta, "tbl_df")) {
    stop("'clean_clea' must be a tibble.", call. = FALSE)
  }

  if (!identical(names(dta), clean_clea_columns())) {
    stop("'clean_clea' has an unexpected schema.", call. = FALSE)
  }

  if (nrow(dta) == 0 || anyNA(dta$id)) {
    stop("'clean_clea' must contain rows and non-missing election IDs.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_simple_systems_schema <- function(dta) {
  if (!inherits(dta, "tbl_df")) {
    stop("'simple_systems' must be a tibble.", call. = FALSE)
  }

  if (!identical(names(dta), simple_systems_columns())) {
    stop("'simple_systems' has an unexpected schema.", call. = FALSE)
  }

  if (!identical(levels(dta$electoral_system), electoral_system_levels())) {
    stop("'simple_systems' has unexpected electoral-system levels.", call. = FALSE)
  }

  invisible(TRUE)
}

validate_simple_systems <- function(dta, tolerance = sqrt(.Machine$double.eps)) {
  validate_simple_systems_schema(dta)

  districts <- split(dta, paste0(dta$id, "_", dta$cst))

  checks <- list(
    "m >= 1" = \(x) all(x$m >= 1),
    "sum(s) == m" = \(x) {
      length(unique(x$m)) == 1 &&
        abs(sum(x$s) - unique(x$m)) <= tolerance
    },
    "sum(v) >= 1" = \(x) sum(x$v) >= 1,
    "ns2 <= m" = \(x) {
      length(unique(x$ns2)) == 1 &&
        length(unique(x$m)) == 1 &&
        unique(x$ns2) <= unique(x$m) + tolerance
    },
    "nv0 >= ns0" = \(x) {
      length(unique(x$nv0)) == 1 &&
        length(unique(x$ns0)) == 1 &&
        unique(x$nv0) + tolerance >= unique(x$ns0)
    },
    "d upper bound" = \(x) {
      length(unique(x$d)) == 1 &&
        length(unique(x$nv2)) == 1 &&
        unique(x$d) <= sqrt((unique(x$nv2) + 1) / (2 * unique(x$nv2))) +
          tolerance
    },
    "d lower bound" = \(x) {
      length(unique(x$d)) == 1 &&
        unique(x$d) + tolerance >=
          (1 / sqrt(2)) * abs(max(x$pv) - max(x$ps))
    },
    "w <= w_max" = \(x) {
      unique(x$w) <=
        w_max(unique(x$nv2), unique(x$nv0), unique(x$ns0)) + tolerance
    },
    "w >= w_min" = \(x) {
      unique(x$w) >=
        w_min(unique(x$nv2), unique(x$nv0), unique(x$ns0)) - tolerance
    },
    "exclusion guarantee" = \(x) {
      # The mathematical bound does not apply when an additional legal
      # threshold is present or unknown. Lebanon's confessional allocation is
      # also not governed by the district-level bound.
      applicable <- !is.na(x$threshold) & !x$threshold & x$iso3 != "LBN"
      all(!applicable | x$s > 0 | x$pv <= x$tx + tolerance)
    },
    "rank-size consistency" = \(x) rank_size(x$v, x$s)
  )

  results <- lapply(checks, function(check) {
    passed <- vapply(districts, check, logical(1))
    names(passed)[!passed]
  })

  tibble::tibble(
    test = names(results),
    failures = lengths(results),
    cases = unname(results)
  )
}

assert_valid_simple_systems <- function(dta, tolerance = sqrt(.Machine$double.eps)) {
  result <- validate_simple_systems(dta, tolerance = tolerance)
  failed <- result[result$failures > 0, ]

  if (nrow(failed) > 0) {
    details <- paste0(failed$test, " (", failed$failures, " failures)")
    stop(
      "'simple_systems' failed validation: ",
      paste(details, collapse = "; "),
      call. = FALSE
    )
  }

  invisible(result)
}
