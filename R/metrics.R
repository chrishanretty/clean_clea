# Internal electoral-system metrics ---------------------------------------

effective_parties <- function(p) {
  1 / sum(p^2)
}

disproportionality <- function(v, s) {
  sqrt(0.5 * sum((v - s)^2, na.rm = TRUE))
}

w_min <- function(nv2, nv0, ns0) {
  x <- (
    nv0 - ns0 -
      sqrt(
        pmax(
          0,
          ns0 * (nv0 - ns0) * (nv0 / nv2 - 1)
        )
      )
  ) / nv0

  pmax(0, x)
}

w_max <- function(nv2, nv0, ns0) {
  len <- max(length(nv2), length(nv0), length(ns0))

  nv2 <- rep(nv2, length.out = len)
  nv0 <- rep(nv0, length.out = len)
  ns0 <- rep(ns0, length.out = len)

  out <- numeric(len)
  valid <- nv2 > 1 & ns0 < nv0 & nv0 > 1

  if (any(valid)) {
    nv2_v <- nv2[valid]
    nv0_v <- nv0[valid]
    ns0_v <- ns0[valid]

    w_one <- ((nv0_v - ns0_v) / nv0_v) * (
      1 -
        sqrt(
          pmax(
            0,
            (1 / (nv0_v - 1)) * (nv0_v / nv2_v - 1)
          )
        )
    )

    c_v <- ceiling(nv2_v)
    w_adj <- numeric(length(nv2_v))
    use_adj <- c_v > ns0_v & c_v > 1

    w_adj[use_adj] <- (
      c_v[use_adj] - ns0_v[use_adj] -
        ns0_v[use_adj] *
          sqrt(
            pmax(
              0,
              (1 / (c_v[use_adj] - 1)) *
                (c_v[use_adj] / nv2_v[use_adj] - 1)
            )
          )
    ) / c_v[use_adj]

    out[valid] <- pmax(w_one, w_adj)
  }

  out
}

rank_size <- function(v, s) {
  sum((outer(v, v, "-") * outer(s, s, "-")) < 0) == 0
}
