### Finland (March 1991)

### Data from https://en.wikipedia.org/wiki/1991_Finnish_parliamentary_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "fin_1991.csv"),
                         show_col_types = FALSE)

### Patch contains information on magnitudes, and seats won by
### Kesk (Suomen Keskusta / Centre Party)
### SDP (Suomen Sosialidemokraattinen Puolue / Social Democratic Party)
### Kok (Kansallinen Kokoomus / National Coalition Party)
### Vas (Vasemmistoliitto / Left Alliance)
### RKP (Ruotsalainen kansanpuolue / Swedish People's Party)
### Vihr (Vihrea liitto / Green League)
### SKL (Suomen Kristillinen Liitto / Finnish Christian League)
### SMP (Suomen Maaseudun Puolue / Finnish Rural Party)
### LKP (Liberaalinen Kansanpuolue / Liberal People's Party)
### L–S–G (Aland liberal list)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 264

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Finland 1991")
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
parties <- c("Kesk", "SDP", "Kok", "Vas", "RKP", "Vihr", "SKL", "SMP", "LKP", "L–S–G")
patch <- patch |>
    mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    mutate(pty = case_when(
                           name == "Kesk" ~ 18,
                           name == "SDP" ~ 24,
                           name == "Kok" ~ 4,
                           name == "Vas" ~ 28,
                           name == "RKP" ~ 11,
                           name == "Vihr" ~ 29,
                           name == "SKL" ~ 19,
                           name == "SMP" ~ 21,
                           name == "LKP" ~ 4000,  ## recoded as others
                           name == "L–S–G" ~ 4000  ## recoded as others
                           )) |>
    dplyr::select(cst, pty, seat) |>
    group_by(cst, pty) |>
    summarize(seat = sum(seat), .groups = "drop") |>
    mutate(id = 264)

dta <- dta |>
    left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    select(-seat.patch)

rm(patch)
