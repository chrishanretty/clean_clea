raw_fixture <- function(
  id = 1,
  cst = c(1, 1),
  mag = 1,
  pty = c(1, 2),
  pv1 = c(10, 5),
  seat = c(1, 0)
) {
  n <- length(cst)

  tibble::tibble(
    release = 20251015,
    id = rep(id, length.out = n),
    rg = "fixture",
    ctr_n = "Fixture",
    ctr = 1,
    yr = 2000,
    mn = 1,
    sub = "fixture",
    cst_n = paste0("District ", cst),
    cst = cst,
    mag = rep(mag, length.out = n),
    pty_n = paste0("Party ", pty),
    pty = pty,
    can = paste0("Candidate ", seq_len(n)),
    pev1 = -990,
    vot1 = -990,
    vv1 = -990,
    ivv1 = -990,
    to1 = -990,
    cv1 = pv1,
    cvs1 = -990,
    pv1 = pv1,
    pvs1 = -990,
    pev2 = -990,
    vot2 = -990,
    vv2 = -990,
    ivv2 = -990,
    to2 = -990,
    cv2 = -990,
    cvs2 = -990,
    pv2 = -990,
    pvs2 = -990,
    seat = seat
  )
}

simple_fixture <- function() {
  tibble::tibble(
    id = c(1, 1),
    iso3 = "FIX",
    country = "Fixture",
    subregion = "Fixture",
    region = "Fixture",
    yr = 2000,
    mn = 1,
    cst_n = "District 1",
    cst = 1,
    m = 1,
    pty_n = c("Party 1", "Party 2"),
    pty = c(1, 2),
    can = c("Candidate 1", "Candidate 2"),
    c = c(60, 40),
    p = c(60, 40),
    v = c(60, 40),
    s = c(1, 0),
    pv = c(0.6, 0.4),
    ps = c(1, 0),
    uncontested = FALSE,
    electoral_system = factor(
      "First-past-the-post in single-member districts",
      levels = cleaclean:::electoral_system_levels()
    ),
    threshold = FALSE,
    nv0 = 2L,
    ns0 = 1L,
    nv2 = 1 / (0.6^2 + 0.4^2),
    ns2 = 1,
    d = sqrt(0.5 * ((0.6 - 1)^2 + (0.4 - 0)^2)),
    w = 0.4,
    tx = 0.5,
    tr = 1 / (1 * (1 / (0.6^2 + 0.4^2))),
    tmin = pmin(tx, tr)
  )
}
