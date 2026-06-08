### Finland (March 1979)

### Data from https://en.wikipedia.org/wiki/1979_Finnish_parliamentary_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "fin_1979.csv"),
                         show_col_types = FALSE)

### Patch contains information on magnitudes, and seats won by
### SDP (Suomen Sosialidemokraattinen Puolue / Social Democratic Party)
### Kok (Kansallinen Kokoomus / National Coalition Party)
### Kesk (Keskustapuolue / Centre Party)
### SKDL (Suomen Kansan Demokraattinen Liitto / Finnish People's Democratic League)
### SKL (Suomen Kristillinen Liitto / Finnish Christian League)
### RKP (Ruotsalainen kansanpuolue / Swedish People's Party)
### SMP (Suomen Maaseudun Puolue / Finnish Rural Party)
### LKP (Liberaalinen Kansanpuolue / Liberal People's Party)
### ÅS (Alandsk Samling / Aland coalition)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 261

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Finland 1979")
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
parties <- c("SDP", "Kok", "Kesk", "SKDL", "SKL", "RKP", "SMP", "LKP", "ÅS")
patch <- patch |>
    mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    mutate(pty = case_when(
                           name == "SDP" ~ 24,
                           name == "Kok" ~ 4,
                           name == "Kesk" ~ 6,
                           name == "SKDL" ~ 15,
                           name == "SKL" ~ 19,
                           name == "RKP" ~ 11,
                           name == "SMP" ~ 21,
                           name == "LKP" ~ 7,
                           name == "ÅS" ~ 4000  ## recoded as others
                           )) |>
    dplyr::select(cst, pty, seat) |>
    group_by(cst, pty) |>
    summarize(seat = sum(seat), .groups = "drop") |>
    mutate(id = 261)

dta <- dta |>
    left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    select(-seat.patch)

rm(patch)
