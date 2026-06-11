### Portugal (October 1980)

### Data from https://en.wikipedia.org/wiki/1980_Portuguese_legislative_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "prt_1980.csv"),
                         show_col_types = FALSE)

### Table contains information on seats won by
### AD / Alianca Democraticá (5001)
### FRS / Frente Republicana e Socialista (5004)
### APU / Alianca Povo Unido (5002)
### PSD / partido social democrata (7)
### PS / partido socialista (8)
### UDP / União Democrática Popular (51)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 554

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Portugal 1980")
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
parties <- c("AD", "FRS", "APU", "PSD", "PS", "UDP")
parties <- paste0(parties, "_mandate")

patch <- patch |>
    dplyr::mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    dplyr::select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    dplyr::mutate(pty = case_when(
                           name == "AD_mandate" ~ 5001,
                           name == "FRS_mandate" ~ 5004,
                           name == "APU_mandate" ~ 5002,
                           name == "PSD_mandate" ~ 7,
                           name == "PS_mandate" ~ 8,
                           name == "UDP_mandate" ~ 51
                           )) |>
    dplyr::select(cst, pty, seat) |>
    dplyr::group_by(cst, pty) |>
    dplyr::summarize(seat = sum(seat), .groups = "drop") |>
    dplyr::mutate(id = 554)

dta <- dta |>
    dplyr::left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    dplyr::mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    dplyr::select(-seat.patch)

rm(patch)


