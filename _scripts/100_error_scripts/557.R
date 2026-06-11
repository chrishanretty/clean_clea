### Portugal (July 1987)

### Data from https://en.wikipedia.org/wiki/1987_Portuguese_legislative_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "prt_1987.csv"),
                         show_col_types = FALSE)

### Table contains information on seats won by
### PSD / partido social democrata (7)
### PS / partido socialista (8)
### CDU / coligacao democrática unitária (5003)
### PRD / partido renovador democrático (6)
### CDS / Centro Democrático e Social (1)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 557

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Portugal 1987")
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
parties <- c("PSD", "PS", "CDU", "PRD", "CDS")
parties <- paste0(parties, "_mandate")

patch <- patch |>
    dplyr::mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    dplyr::select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    dplyr::mutate(pty = case_when(
                           name == "PSD_mandate" ~ 7,
                           name == "PS_mandate" ~ 8,
                           name == "CDU_mandate" ~ 5003,
                           name == "PRD_mandate" ~ 6,
                           name == "CDS_mandate" ~ 1
                           )) |>
    dplyr::select(cst, pty, seat) |>
    dplyr::group_by(cst, pty) |>
    dplyr::summarize(seat = sum(seat), .groups = "drop") |>
    dplyr::mutate(id = 557)

dta <- dta |>
    dplyr::left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    dplyr::mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    dplyr::select(-seat.patch)

rm(patch)
