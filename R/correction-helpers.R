# Internal helpers used by election-specific correction scripts -----------

collapse_netherlands <- function(dta, id) {
  netherlands_id <- dplyr::filter(dta, id == !!id)

  if (all(netherlands_id$pty == -990 | is.na(netherlands_id$pty))) {
    stop(
      "Netherlands election ",
      id,
      " does not have usable party coding for a party-level collapse.",
      call. = FALSE
    )
  }

  district_party_id <- netherlands_id |>
    dplyr::filter(pty != -990) |>
    dplyr::group_by(
      release, id, rg, ctr_n, ctr, yr, mn, sub, cst, pty_n, pty
    ) |>
    dplyr::summarise(
      pv1 = if (all(pv1 == -990 | is.na(pv1))) {
        -990
      } else {
        max(pv1[pv1 != -990 & !is.na(pv1)], na.rm = TRUE)
      },
      seat = sum(seat[seat > 0], na.rm = TRUE),
      .groups = "drop"
    )

  total_mag <- netherlands_id |>
    dplyr::distinct(cst, mag) |>
    dplyr::filter(mag > 0) |>
    dplyr::summarise(mag = sum(mag, na.rm = TRUE), .groups = "drop") |>
    dplyr::pull(mag)

  if (length(total_mag) == 0 || is.na(total_mag) || total_mag == 0) {
    total_mag <- district_party_id |>
      dplyr::summarise(
        mag = sum(seat[seat > 0], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::pull(mag)
  }

  if (length(total_mag) == 0 || is.na(total_mag) || total_mag == 0) {
    total_mag <- -990
  }

  nationwide_id <- district_party_id |>
    dplyr::group_by(release, id, rg, ctr_n, ctr, yr, mn, sub, pty_n, pty) |>
    dplyr::summarise(
      pv1 = if (all(pv1 == -990 | is.na(pv1))) {
        -990
      } else {
        sum(pv1[pv1 != -990 & !is.na(pv1)], na.rm = TRUE)
      },
      seat = sum(seat, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cst_n = "nationwide district",
      cst = 901,
      mag = total_mag,
      can = "-990",
      pev1 = -990,
      vot1 = -990,
      vv1 = -990,
      ivv1 = -990,
      to1 = -990,
      cv1 = -990,
      cvs1 = -990,
      pvs1 = -990,
      pev2 = -990,
      vot2 = -990,
      vv2 = -990,
      ivv2 = -990,
      to2 = -990,
      cv2 = -990,
      cvs2 = -990,
      pv2 = -990,
      pvs2 = -990
    ) |>
    dplyr::select(
      release, id, rg, ctr_n, ctr, yr, mn, sub, cst_n, cst, mag, pty_n, pty,
      can, pev1, vot1, vv1, ivv1, to1, cv1, cvs1, pv1, pvs1, pev2, vot2, vv2,
      ivv2, to2, cv2, cvs2, pv2, pvs2, seat
    )

  dta |>
    dplyr::filter(id != !!id) |>
    dplyr::bind_rows(nationwide_id)
}

collapse_cyprus <- function(dta, id) {
  cyprus_id <- dta |>
    dplyr::filter(id == !!id) |>
    dplyr::select(
      id, ctr_n, ctr, yr, mn, cst_n, cst, mag, pty_n, pty, pv1, seat
    ) |>
    dplyr::group_by(id, cst_n, cst, pty_n, pty) |>
    dplyr::mutate(seat = sum(seat, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  dta |>
    dplyr::filter(id != !!id) |>
    dplyr::bind_rows(cyprus_id)
}

collapse_nz_list <- function(
  dta,
  id,
  mag,
  seat_allocations = NULL,
  arrange_votes = FALSE
) {
  nz_list <- dta |>
    dplyr::filter(id == !!id, cst >= 900) |>
    dplyr::group_by(release, id, rg, ctr_n, ctr, yr, mn, pty_n, pty) |>
    dplyr::summarise(pv1 = sum(pv1), .groups = "drop") |>
    dplyr::mutate(
      mag = mag,
      cv1 = -990,
      seat = 0,
      cst = 900,
      cst_n = "national list"
    )

  if (arrange_votes) {
    nz_list <- dplyr::arrange(nz_list, dplyr::desc(pv1))
  }

  if (!is.null(seat_allocations)) {
    seat_index <- match(nz_list$pty, as.numeric(names(seat_allocations)))
    matched <- !is.na(seat_index)
    nz_list$seat[matched] <- unname(seat_allocations[seat_index[matched]])
  }

  dta |>
    dplyr::filter(!(id == !!id & cst >= 900)) |>
    dplyr::bind_rows(nz_list)
}

impute_fptp_seats <- function(dta, id, csts = NULL, votes = "pv1") {
  if (is.null(csts)) {
    csts <- unique(dta$cst[dta$id == id])
  }

  dta |>
    dplyr::group_by(id, cst) |>
    dplyr::mutate(
      seat = dplyr::case_when(
        id == !!id &
          cst %in% csts &
          .data[[votes]] == max(.data[[votes]], na.rm = TRUE) ~ 1,
        id == !!id & cst %in% csts ~ 0,
        TRUE ~ seat
      )
    ) |>
    dplyr::ungroup()
}
