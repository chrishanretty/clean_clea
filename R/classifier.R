electoral_system_levels <- function() {
  c(
    "First-past-the-post in single-member districts",
    "Two-round system in single-member districts",
    "Alternative vote in single-member districts",
    "Block vote in multi-member districts",
    "Party block vote in multi-member districts",
    "Parallel (SMD/PR)",
    "Mixed-member proportional (SMD with PR compensatory seats)",
    "List PR with small multi-member districts",
    "List PR with large multi-member districts",
    "Single-transferable vote in multi-member districts",
    "Single non-transferable vote in multi-member districts",
    "Limited vote in multi-member districts",
    "Borda Count in single- or multi-member districts"
  )
}

build_electoral_system_classifier <- function(vdem, missing_electoral_systems) {
  classifier <- vdem |>
    dplyr::select(
      iso3 = country_text_id,
      year,
      electoral_system = v2elloelsy,
      upper_tier = v2elloupdis,
      threshold = v2elthresh
    ) |>
    tibble::as_tibble()

  missing_electoral_systems <- dplyr::arrange(
    missing_electoral_systems,
    iso3,
    year
  )

  for (i in seq_len(nrow(missing_electoral_systems))) {
    index <- classifier$iso3 == missing_electoral_systems$iso3[i] &
      classifier$year == missing_electoral_systems$year[i]

    if (!any(index)) {
      classifier <- dplyr::bind_rows(
        classifier,
        missing_electoral_systems[i, ]
      )
    } else {
      classifier$electoral_system[index] <-
        missing_electoral_systems$electoral_system[i]
    }
  }

  classifier$electoral_system[
    classifier$iso3 == "CIV" & !is.na(classifier$electoral_system)
  ] <- 3
  classifier$electoral_system[
    classifier$iso3 == "BRB" &
      classifier$year <= 1970 &
      !is.na(classifier$electoral_system)
  ] <- 3
  classifier$electoral_system[
    classifier$iso3 == "PNG" & classifier$year == 1972
  ] <- 2
  classifier$electoral_system[
    classifier$iso3 == "SGP" & classifier$year == 1948
  ] <- 4
  classifier$electoral_system[
    classifier$iso3 == "SGP" & classifier$year %in% c(1951, 1955, 1959)
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "LKA" & classifier$year == 1947
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "ZAF" & classifier$year == 1984
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "ZMB" & classifier$year == 1964
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "NZL" & classifier$year == 1943
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "IND" & classifier$year %in% c(1985, 1992)
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "KEN" & classifier$year == 1957
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "PHL" & classifier$year == 1969
  ] <- 0
  classifier$electoral_system[
    classifier$iso3 == "MDG" & classifier$year == 2014
  ] <- 5
  classifier$electoral_system[
    classifier$iso3 == "DOM" & classifier$year == 1962
  ] <- 7
  classifier$electoral_system[
    classifier$iso3 == "KWT" &
      classifier$year %in% c(
        1963, 1967, 1971, 1975, 1981, 1985, 1990, 1992, 1996, 2019
      )
  ] <- 10
  classifier$electoral_system[
    classifier$iso3 == "HND" & classifier$year %in% 1980:1981
  ] <- 7
  classifier$electoral_system[
    classifier$iso3 == "ECU" & classifier$year == 1998
  ] <- 7

  classifier <- classifier |>
    dplyr::mutate(
      electoral_system = factor(
        electoral_system,
        levels = 0:12,
        labels = electoral_system_levels()
      ),
      simple_system = dplyr::case_when(
        is.na(electoral_system) ~ NA,
        electoral_system %in% c(
          "First-past-the-post in single-member districts",
          "List PR with small multi-member districts",
          "List PR with large multi-member districts",
          "Single non-transferable vote in multi-member districts"
        ) & (upper_tier == 0 | is.na(upper_tier)) ~ TRUE,
        TRUE ~ FALSE
      )
    ) |>
    dplyr::relocate(simple_system, .after = electoral_system) |>
    dplyr::select(-upper_tier)

  classifier$simple_system[classifier$iso3 %in% c("URY", "SVN", "ARG", "AUT")] <-
    FALSE
  classifier$simple_system[
    classifier$iso3 == "ROU" & classifier$year %in% 1990:2007
  ] <- FALSE
  classifier$simple_system[
    classifier$iso3 == "IDN" & classifier$year >= 2004
  ] <- FALSE
  classifier$simple_system[
    classifier$iso3 == "EST" & classifier$year >= 1992
  ] <- FALSE
  classifier$simple_system[classifier$iso3 %in% c("LVA", "ECU")] <- FALSE

  dplyr::mutate(
    classifier,
    threshold = dplyr::if_else(threshold > 0, TRUE, FALSE)
  )
}
