### Poland (September 2001)

### Data from https://en.wikipedia.org/wiki/2001_Polish_parliamentary_election#Seat_distribution_by_constituency

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "pol_2001.csv"),
                         show_col_types = FALSE)

### SLD (5003)
### PO (141)
### SRP (142)
### PiS (140)
### PSL (62)
### LPR (138)
### MN (35)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 995

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Poland 2001")
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
parties <- c("SLD", "PO", "SRP", "PiS", "PSL", "LPR", "MN")

patch <- patch |>
    dplyr::mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    dplyr::select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    dplyr::mutate(pty = case_when(
                      name == "SLD" ~ 5003,
                      name == "PO" ~ 141,
                      name == "SRP" ~ 142,
                      name == "PiS" ~ 140,
                      name == "PSL" ~ 62,
                      name == "LPR" ~ 138,
                      name == "MN" ~ 35
                           )) |>
    dplyr::select(cst, pty, seat) |>
    dplyr::group_by(cst, pty) |>
    dplyr::summarize(seat = sum(seat), .groups = "drop") |>
    dplyr::mutate(id = 995)
