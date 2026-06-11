### Portugal (February 2005)

### Data from https://en.wikipedia.org/wiki/2005_Portuguese_legislative_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "prt_2005.csv"),
                         show_col_types = FALSE)

### Table contains information on seats won by
### PS / partido socialista (8)
### PSD / partido social democrata (7)
### CDU / coligacao democrática unitária (5003)
### CDSPP / CDS - Partido Popular (1)
### BE / bloco de esquerda (9)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 999

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Portugal 2005")
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
parties <- c("PS", "PSD", "CDU", "CDSPP", "BE")
parties <- paste0(parties, "_mandate")

patch <- patch |>
    dplyr::mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    dplyr::select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    dplyr::mutate(pty = case_when(
                           name == "PS_mandate" ~ 8,
                           name == "PSD_mandate" ~ 7,
                           name == "CDU_mandate" ~ 5003,
                           name == "CDSPP_mandate" ~ 1,
                           name == "BE_mandate" ~ 9
                           )) |>
    dplyr::select(cst, pty, seat) |>
    dplyr::group_by(cst, pty) |>
    dplyr::summarize(seat = sum(seat), .groups = "drop") |>
    dplyr::mutate(id = 999)

dta <- dta |>
    dplyr::left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    dplyr::mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    dplyr::select(-seat.patch)

rm(patch)
