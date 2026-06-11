### Finland (March 1995)

### Data from https://en.wikipedia.org/wiki/1995_Finnish_parliamentary_election

patch <- readr::read_csv(here::here("_data",
                                    "man",
                                    "fin_1995.csv"),
                         show_col_types = FALSE)

### Patch contains information on magnitudes, and seats won by

### SDP (Social Democratic Party) 
### Kesk (Suomen Keskusta / Center Party)
### Kok (Kansallinen Kokoomus / National Coalition party)
### Vas (Vasemmistoliitto / Left Alliance)
### RKP (Ruotsalainen kansanpuolue / Swedish People's Party)
### Vihr (Vihreä liitto / Green League)
### SKL (Suomen Kristillisdemokraatit / Christian Democrats)
### Nuor (Nuorsuomalaiset / Young Finns) 
### SMP (Suomen maaseudun puolue / Finnish Rural Party) 
### EKO (Ekologinen puolue Vihreät / Ecological Party)
### L (Liberalerna på Åland / Liberals for Åland)

### Patch magnitudes first
patch_mags <- patch[, c("cst", "mag")]
### Use CLEA election id
patch_mags$id <- 265

if (anyDuplicated(patch_mags[, c("cst")])) {
    stop("Duplicate entries in patches for Finland 1995")
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
parties <- c("SDP", "Kesk", "Kok", "Vas", "RKP", "Vihr", "SKL", "Nuor", "SMP", "EKO", "L")
patch <- patch |>
    mutate(across(all_of(parties), \(x) coalesce(x, 0L))) |>
    select(cst, all_of(parties)) |>
    tidyr::pivot_longer(cols = all_of(parties),
                        values_to = "seat") |>
    mutate(pty = case_when(name == "SDP" ~ 24,
                           name == "Kesk" ~ 18,
                           name == "Kok" ~ 4,
                           name == "Vas" ~ 28,
                           name == "RKP" ~ 11,
                           name == "Vihr" ~ 29,
                           name == "SKL" ~ 19,
                           name == "Nuor" ~ 4000, ## recoded as "others"
                           name == "SMP" ~ 4000,
                           name == "EKO" ~ 4000,
                           name == "L" ~ 4000)) |>
    dplyr::select(cst, pty, seat) |>
    group_by(cst, pty) |>
    summarize(seat = sum(seat), .groups = "drop") |>
    mutate(id = 265)

dta <- dta |>
    left_join(patch,
              by = join_by(id, cst, pty),
              suffix = c("", ".patch"),
              relationship = "one-to-one") |>
    mutate(seat = case_when((is.na(seat) | seat == -990) ~ seat.patch,
                            TRUE ~ seat)) |>
    select(-seat.patch)

rm(patch)
