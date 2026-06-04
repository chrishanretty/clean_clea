build_simple_systems <- function(clean_clea, classifier, un_geoscheme) {
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    stop("Package 'countrycode' is required to rebuild the data.", call. = FALSE)
  }

  dta <- clean_clea |>
    dplyr::filter(yr >= 1900) |>
    dplyr::select(
      id,
      ctr,
      yr,
      mn,
      cst_n,
      cst,
      m = mag,
      pty_n,
      pty,
      can,
      c = cv1,
      p = pv1,
      s = seat
    ) |>
    dplyr::mutate(
      iso3 = suppressWarnings(
        dplyr::case_when(
          ctr == 1001 ~ "TWN",
          ctr == 1002 ~ "XKX",
          ctr == 1003 ~ "SML",
          TRUE ~ countrycode::countrycode(ctr, "un", "iso3c")
        )
      )
    ) |>
    dplyr::relocate(iso3, .after = id) |>
    dplyr::select(-ctr) |>
    dplyr::left_join(
      dplyr::select(un_geoscheme, -m49, -iso2),
      by = "iso3"
    ) |>
    dplyr::relocate(country, .after = iso3) |>
    dplyr::relocate(subregion, .after = country) |>
    dplyr::relocate(region, .after = subregion) |>
    dplyr::filter(!(p == -994 & c == -994)) |>
    dplyr::mutate(
      uncontested = dplyr::if_else(p == -992 & c == -992, TRUE, FALSE),
      c = dplyr::if_else(uncontested, 1, c),
      p = dplyr::if_else(uncontested, 1, p)
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        \(x) dplyr::if_else(x < 0, NA, x)
      ),
      dplyr::across(
        dplyr::where(is.character),
        \(x) dplyr::if_else(x %in% c("-990", "-992"), NA_character_, x)
      )
    ) |>
    dplyr::group_by(id, cst, pty) |>
    dplyr::mutate(party_n = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      v = dplyr::case_when(
        party_n > 1 ~ c,
        is.na(c) & !is.na(p) ~ p,
        TRUE ~ c
      )
    ) |>
    dplyr::relocate(v, .before = s) |>
    dplyr::select(-party_n) |>
    dplyr::left_join(classifier, by = dplyr::join_by(iso3, yr == year)) |>
    dplyr::filter(simple_system) |>
    dplyr::select(-simple_system)

  pr <- dta |>
    dplyr::filter(
      electoral_system %in% c(
        "List PR with small multi-member districts",
        "List PR with large multi-member districts"
      )
    ) |>
    dplyr::group_by(
      id, iso3, country, subregion, region, yr, mn, cst_n, cst, pty_n, pty
    ) |>
    dplyr::summarise(
      m = unique(m),
      v = sum(v),
      s = sum(s),
      uncontested = unique(uncontested),
      electoral_system = unique(electoral_system),
      threshold = unique(threshold),
      .groups = "drop"
    )

  dta <- dta |>
    dplyr::filter(
      !electoral_system %in% c(
        "List PR with small multi-member districts",
        "List PR with large multi-member districts"
      )
    ) |>
    dplyr::bind_rows(pr) |>
    dplyr::filter(!iso3 %in% c("USA", "PAN"))

  dta$m[
    is.na(dta$m) &
      dta$electoral_system ==
        "First-past-the-post in single-member districts"
  ] <- 1

  dta <- dta |>
    dplyr::group_by(id, cst) |>
    dplyr::mutate(
      s = dplyr::case_when(
        m == 1 & all(is.na(s)) & all(!is.na(v)) & v == max(v) ~ 1,
        m == 1 & all(is.na(s)) & all(!is.na(v)) ~ 0,
        TRUE ~ s
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(id, cst) |>
    dplyr::mutate(
      m = ifelse(all(is.na(m)) & all(!is.na(s)), sum(s), m)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(id) |>
    dplyr::filter(!any(is.na(v)), !any(is.na(s))) |>
    dplyr::filter(!any(pty == 4000), !any(pty == 6000)) |>
    dplyr::ungroup() |>
    dplyr::group_by(id, cst) |>
    dplyr::mutate(rank_size = rank_size(v, s)) |>
    dplyr::group_by(id) |>
    dplyr::filter(all(rank_size)) |>
    dplyr::select(-rank_size) |>
    dplyr::group_by(id, cst) |>
    dplyr::mutate(
      pv = v / sum(v),
      ps = s / sum(s)
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate(pv, .after = s) |>
    dplyr::relocate(ps, .after = pv) |>
    dplyr::arrange(id, cst, dplyr::desc(pv), dplyr::desc(ps))

  compute_summary_statistics(dta)
}

compute_summary_statistics <- function(dta) {
  dta |>
    dplyr::group_by(id, cst) |>
    dplyr::mutate(
      nv0 = sum(pv > 0),
      ns0 = sum(ps > 0),
      nv2 = effective_parties(pv),
      ns2 = effective_parties(ps),
      d = disproportionality(pv, ps),
      w = sum(pv[s == 0])
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      tx = 1 / (m + 1),
      tr = 1 / (m * nv2),
      tmin = pmin(tx, tr)
    )
}
