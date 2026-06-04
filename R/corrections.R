correction_script_environment <- function(data_raw_dir) {
  required_packages <- c("dplyr", "tidyr", "stringr", "tibble", "readr")

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Install the following packages before rebuilding the data: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  out <- new.env(parent = baseenv())

  list2env(
    list(
      across = dplyr::across,
      add_row = tibble::add_row,
      bind_rows = dplyr::bind_rows,
      case_when = dplyr::case_when,
      coalesce = dplyr::coalesce,
      desc = dplyr::desc,
      ends_with = dplyr::ends_with,
      filter = dplyr::filter,
      first = dplyr::first,
      group_by = dplyr::group_by,
      if_else = dplyr::if_else,
      join_by = dplyr::join_by,
      left_join = dplyr::left_join,
      mutate = dplyr::mutate,
      pivot_longer = tidyr::pivot_longer,
      read_csv = readr::read_csv,
      row_number = dplyr::row_number,
      select = dplyr::select,
      str_detect = stringr::str_detect,
      tibble = tibble::tibble,
      ungroup = dplyr::ungroup,
      collapse_netherlands = collapse_netherlands,
      collapse_cyprus = collapse_cyprus,
      collapse_nz_list = collapse_nz_list,
      impute_fptp_seats = impute_fptp_seats,
      data_raw_dir = data_raw_dir
    ),
    envir = out
  )

  out
}

apply_corrections <- function(dta, corrections_dir, data_raw_dir) {
  correction_paths <- sort(
    list.files(corrections_dir, pattern = "[.]R$", full.names = TRUE)
  )

  if (length(correction_paths) == 0) {
    stop("No election correction scripts were found.", call. = FALSE)
  }

  id_order <- unique(as.character(dta$id))
  split_dta <- split(dta, dta$id)
  script_parent <- correction_script_environment(data_raw_dir)

  for (path in correction_paths) {
    target_ids <- tools::file_path_sans_ext(basename(path))
    script_dta <- dplyr::bind_rows(split_dta[target_ids])
    script_env <- list2env(list(dta = script_dta), parent = script_parent)

    sys.source(path, envir = script_env, keep.source = FALSE)

    corrected <- split(script_env$dta, script_env$dta$id)

    for (target_id in target_ids) {
      if (target_id %in% names(corrected)) {
        split_dta[[target_id]] <- corrected[[target_id]]
      } else {
        split_dta[[target_id]] <- NULL
      }
    }
  }

  dplyr::bind_rows(
    split_dta[c(
      id_order[id_order %in% names(split_dta)],
      setdiff(names(split_dta), id_order)
    )]
  )
}
