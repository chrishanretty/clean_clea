### Finland (March 1954)

### Data from https://en.wikipedia.org/wiki/1954_Finnish_parliamentary_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "fin_1954.csv"),
                         show_col_types = FALSE)

### Patch contains information on magnitudes, and seats won by
### SDP (Suomen Sosialidemokraattinen Puolue / Social Democratic Party)
### ML (Maalaisliitto / Agrarian League)
### SKDL (Suomen Kansan Demokraattinen Liitto / Finnish People's Democratic League)
### Kok (Kansallinen Kokoomus / National Coalition Party)
### SK (Suomen Kansanpuolue / Finnish People's Party)
### RKP (Ruotsalainen kansanpuolue / Swedish People's Party)
### ÅS (Alandsk Samling / Aland coalition)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 254

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Finland 1954")
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
parties <- c("SDP", "ML", "SKDL", "Kok", "SK", "RKP", "ÅS")
patch <- patch |>
    mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    mutate(pty = case_when(
                           name == "SDP" ~ 24,
                           name == "ML" ~ 8,
                           name == "SKDL" ~ 15,
                           name == "Kok" ~ 4,
                           name == "SK" ~ 17,
                           name == "RKP" ~ 11,
                           name == "ÅS" ~ 4000  ## recoded as others
                           )) |>
    dplyr::select(cst, pty, seat) |>
    group_by(cst, pty) |>
    summarize(seat = sum(seat), .groups = "drop") |>
    mutate(id = 254)

dta <- dta |>
    left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    select(-seat.patch)

rm(patch)
