### Norway (October 1927)

### Data from https://en.wikipedia.org/wiki/1927_Norwegian_parliamentary_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "nor_1927.csv"),
                         show_col_types = FALSE) |>
    dplyr::rename_with(trimws)

### Table contains information on seats won by
### Ap / Det norske Arbeiderparti (5)
### H–FV / Høyre og Frisinnede Venstre (11)
### V / Venstre (25)
### B / Bondepartiet (3)
### K / Norske Kommunistiske Parti (17)
### RF / Radikale Folkeparti (18)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 533

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Norway 1927")
}

dta <- dta |>
    left_join(patch_mags,
              by = join_by(id, cst),
              suffix = c("", ".patched"),
              relationship = "many-to-one") |>
    mutate(mag = case_when((is.na(mag) | mag == -990) ~ mag.patched,
                           TRUE ~ mag)) |>
    select(-mag.patched)

rm(patch_mags)


## Now patch party totals
parties <- c("Ap", "H–FV", "V", "B", "K", "RF")


patch <- patch |>
    dplyr::mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    dplyr::select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    dplyr::mutate(pty = case_when(
                      name == "Ap" ~ 5,
                      name == "H–FV" ~ 11,
                      name == "V" ~ 25,
                      name == "B" ~ 3,
                      name == "K" ~ 17,
                      name == "RF" ~ 18,
                      )) |>
    dplyr::select(cst, pty, seat) |>
    dplyr::group_by(cst, pty) |>
    dplyr::summarize(seat = sum(seat), .groups = "drop") |>
    dplyr::mutate(id = 533)

dta <- dta |>
    dplyr::left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    dplyr::mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    dplyr::select(-seat.patch)

rm(patch)
