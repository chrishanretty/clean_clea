### Paraguay (April 2003)

### Data from https://web.archive.org/web/20150407061328/https://tsje.gov.py/elecciones-generales-2003.html

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "pry_2003.csv"),
                         show_col_types = FALSE)

### Table contains information on seats won by
### ANR (Asn. Nacional Republicana / Partito Colorado) - 1
### PLRA (Partido Liberal Radical Autentico) - 34
### UNACE (Union Nacional de Ciudadanos Eticos ) - 40
### MPQ (Patria Querida) - 37
### PPS (Partido Pais Solidario) - 35

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 1269

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Paraguay 2003")
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
parties <- c("ANR", "PLRA", "UNACE", "MPQ", "PPS")

patch <- patch |>
    dplyr::mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    dplyr::select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    dplyr::mutate(pty = case_when(
                           name == "ANR" ~ 1,
                           name == "PLRA" ~ 34,
                           name == "UNACE" ~ 40,
                           name == "MPQ" ~ 37,
                           name == "PPS" ~ 35
                           )) |>
    dplyr::select(cst, pty, seat) |>
    dplyr::group_by(cst, pty) |>
    dplyr::summarize(seat = sum(seat), .groups = "drop") |>
    dplyr::mutate(id = 1269)

dta <- dta |>
    dplyr::left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    dplyr::mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    dplyr::select(-seat.patch)

rm(patch)


