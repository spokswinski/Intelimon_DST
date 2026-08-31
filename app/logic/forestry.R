# app/logic/forestry.R
# ---------------------------------------------------------------------------
# Forestry analytics. Pure functions on the per-scan tree inventory; no Shiny.
#
# OCCLUSION SCALING (important): a scan's plot is a circle of `radius` metres,
# but TLS occlusion means stems are only findable over part of it. The metrics
# response reports `nonocarea` (m^2) - the unoccluded area - per scan. EVERY
# per-area value (density, basal area, SDI, and the FVS expansion factor)
# divides by nonocarea, not by pi*r^2. Using the full plot area understates
# density and basal area by the occluded fraction. `stand_metrics()` therefore
# takes the scaling area explicitly and defaults to occlusion-corrected.
#
# UNITS: the tree inventory reports DBH in INCHES (already converted upstream)
# and X / Y / H in metres. Per-stem basal area is the forester's constant form
# BA_ft2 = 0.005454 * DBH_in^2; the inventory's own `BasalA` column carries
# exactly that and is used directly when present.
#
# Note QMD is a ratio of stem sizes and is unaffected by the scaling area;
# only per-area quantities move.
#
# Residual bias: occlusion is size- and distance-biased (small stems behind
# large boles at the plot edge go missing first), so area correction alone
# still leaves the smallest diameter classes under-counted. detection_shortfall()
# estimates that remainder for display; it is indicative, not a correction.
# ---------------------------------------------------------------------------

box::use(
  data.table[data.table, as.data.table, rbindlist, setorder],
  stats[rnorm, runif, sd],
)

# ---- unit conversions -----------------------------------------------------
IN_TO_CM  <- 2.54
M_TO_FT   <- 3.280840
FT2_TO_M2 <- 0.09290304
HA_TO_AC  <- 2.471054
SQM_TO_HA <- 1e-4

#' Forester's constant: basal area (ft^2) = 0.005454 * DBH_inches^2
#' @export
FORESTERS_CONSTANT <- 0.005454

#' Per-stem basal area (ft^2) from DBH in inches.
#' @export
tree_basal_area_ft2 <- function(dbh_in) FORESTERS_CONSTANT * dbh_in^2

#' Per-stem basal area (ft^2), preferring the inventory's BasalA column.
#' Falls back to the forester's constant when BasalA is absent or unusable.
#' @export
stem_ba_ft2 <- function(trees) {
  dbh <- suppressWarnings(as.numeric(trees$DBH))
  ba  <- if (!is.null(trees$BasalA)) suppressWarnings(as.numeric(trees$BasalA))
         else rep(NA_real_, length(dbh))
  bad <- is.na(ba) | ba <= 0
  ba[bad] <- tree_basal_area_ft2(dbh[bad])
  ba
}

#' Full (unoccluded-maximum) plot area in m^2 for a given radius.
#' @export
plot_area_m2 <- function(radius_m) pi * radius_m^2

#' Resolve the area (m^2) used to scale per-area values for one scan.
#' `mode` is "nonoc" (default, occlusion-corrected) or "full".
#' Falls back to the full plot area when nonocarea is missing or nonsensical.
#' @export
scaling_area_m2 <- function(nonocarea, radius_m, mode = c("nonoc", "full")) {
  mode <- match.arg(mode)
  full <- plot_area_m2(radius_m)
  if (mode == "full") return(full)
  a <- suppressWarnings(as.numeric(nonocarea))
  if (length(a) != 1 || is.na(a) || a <= 0 || a > full * 1.05) return(full)
  a
}

#' Stand-level metrics for one set of stems.
#'
#' @param trees data.table/data.frame with DBH (inches), H (m); optional BasalA, cr.
#' @param area_m2 scaling area - pass the value from scaling_area_m2().
#' @return named list of stand metrics (both imperial and metric members).
#' @export
stand_metrics <- function(trees, area_m2) {
  n <- nrow(trees)
  if (n == 0 || is.na(area_m2) || area_m2 <= 0) {
    return(list(n = 0L, tpa = NA_real_, tph = NA_real_, ba_ac = NA_real_,
                ba_ha = NA_real_, qmd_in = NA_real_, qmd_cm = NA_real_,
                sdi = NA_real_, rd = NA_real_, h_dom_m = NA_real_,
                cr_mean = NA_real_, area_ac = NA_real_, ef_tpa = NA_real_))
  }

  dbh_in <- suppressWarnings(as.numeric(trees$DBH))   # inventory DBH is INCHES
  ht_m   <- suppressWarnings(as.numeric(trees$H))
  ba_ft2 <- stem_ba_ft2(trees)
  ok <- !is.na(dbh_in) & dbh_in > 0
  dbh_in <- dbh_in[ok]; ht_m <- ht_m[ok]; ba_ft2 <- ba_ft2[ok]
  n <- length(dbh_in)
  if (n == 0) return(stand_metrics(trees[0], area_m2))

  area_ha <- area_m2 * SQM_TO_HA
  area_ac <- area_ha * HA_TO_AC

  ba_tot_ft2 <- sum(ba_ft2, na.rm = TRUE)        # summed per-stem BasalA
  qmd_in <- sqrt(mean(dbh_in^2))                 # DBH already inches
  qmd_cm <- qmd_in * IN_TO_CM

  tph <- n / area_ha
  tpa <- n / area_ac
  ba_ac <- ba_tot_ft2 / area_ac
  ba_ha <- (ba_tot_ft2 * FT2_TO_M2) / area_ha

  # Reineke stand density index and Curtis relative density (imperial)
  sdi <- tpa * (qmd_in / 10)^1.605
  rd  <- ba_ac / sqrt(qmd_in)

  # dominant height: mean of the tallest 20%
  h_ok <- ht_m[!is.na(ht_m)]
  h_dom <- if (length(h_ok) > 0) {
    k <- max(1L, round(length(h_ok) * 0.2))
    mean(sort(h_ok, decreasing = TRUE)[seq_len(k)])
  } else NA_real_

  cr_mean <- if (!is.null(trees$cr)) mean(suppressWarnings(
    as.numeric(trees$cr))[ok], na.rm = TRUE) else NA_real_

  list(
    n = n, tpa = tpa, tph = tph, ba_ac = ba_ac, ba_ha = ba_ha,
    qmd_in = qmd_in, qmd_cm = qmd_cm, sdi = sdi, rd = rd,
    h_dom_m = h_dom, cr_mean = cr_mean,
    area_ac = area_ac, area_ha = area_ha,
    ef_tpa = 1 / area_ac          # FVS TreeCount: trees/acre per stem
  )
}

#' Crown ratio per stem from height and canopy base height.
#' CBH is a scan-level metric, so it is applied as the stand crown base unless
#' a per-tree value is supplied. Returns NA where inputs are unusable.
#' @export
crown_ratio <- function(ht_m, cbh_m) {
  ht <- suppressWarnings(as.numeric(ht_m))
  cb <- suppressWarnings(as.numeric(cbh_m))
  cr <- (ht - cb) / ht
  cr[!is.finite(cr) | cr <= 0 | cr > 1] <- NA_real_
  cr
}

#' Estimated detection probability by stem size (indicative).
#' Small stems are preferentially occluded; this expresses the residual bias
#' that area correction does not remove. Not applied to metrics - display only.
#' @export
detection_prob <- function(dbh_in, visible_fraction = 1) {
  vf <- max(0.05, min(1, visible_fraction))
  p  <- 0.42 + 0.55 * (1 - exp(-dbh_in / 6.3))   # DBH in inches
  pmin(0.99, p * (0.6 + 0.4 * vf))
}

#' Per-DBH-class detected vs inferred-missing counts (display only).
#' @export
detection_shortfall <- function(dbh_in, visible_fraction = 1, class_width = 2) {
  d <- dbh_in[!is.na(dbh_in) & dbh_in > 0]
  if (length(d) == 0) return(data.table())
  cls <- floor(d / class_width) * class_width
  tab <- data.table(cls = cls)[, .(detected = .N), by = cls]
  setorder(tab, cls)
  p <- detection_prob(tab$cls + class_width / 2, visible_fraction)
  tab[, inferred_total := detected / p]
  tab[, missing := pmax(0, inferred_total - detected)]
  tab[]
}

# ---- species assignment ---------------------------------------------------

#' Assign species codes to stems from a composition ratio.
#'
#' Lidar does not measure species, so a ratio fixes stand composition but not
#' which stem is which. `rule` decides that:
#'   "random"  - independent draw; matches the ratio, implies no structure
#'   "size"    - largest stems to the first species (overstory/understory)
#'   "cluster" - species occupy spatial patches
#' Stand-level proportions match the ratio under all three; per-tree identity
#' (and therefore FVS growth) differs.
#'
#' @param trees data.table with DBH, X, Y
#' @param species data.frame/list with `code` and `pct`
#' @return character vector of species codes, length nrow(trees)
#' @export
assign_species <- function(trees, species, rule = c("random", "size", "cluster"),
                           seed = 42) {
  rule <- match.arg(rule)
  n <- nrow(trees)
  if (n == 0) return(character())

  sp <- as.data.table(species)
  sp <- sp[!is.na(code) & nzchar(as.character(code)) &
             suppressWarnings(as.numeric(pct)) > 0]
  if (nrow(sp) == 0) return(rep(NA_character_, n))

  w <- as.numeric(sp$pct); w <- w / sum(w)
  cum <- cumsum(w)
  codes <- as.character(sp$code)

  set.seed(seed)

  if (rule == "random") {
    u <- runif(n)
    idx <- findInterval(u, c(0, cum), rightmost.closed = TRUE)
    idx[idx < 1] <- 1L; idx[idx > length(codes)] <- length(codes)
    return(codes[idx])
  }

  # deterministic orderings: allocate in blocks matching the ratio
  ord <- switch(
    rule,
    size    = order(-suppressWarnings(as.numeric(trees$DBH))),
    cluster = order(atan2(suppressWarnings(as.numeric(trees$Y)),
                          suppressWarnings(as.numeric(trees$X))))
  )
  f <- (seq_len(n) - 0.5) / n
  idx <- findInterval(f, c(0, cum), rightmost.closed = TRUE)
  idx[idx < 1] <- 1L; idx[idx > length(codes)] <- length(codes)

  out <- character(n)
  out[ord] <- codes[idx]
  out
}

# ---- thinning -------------------------------------------------------------

#' Mark stems for removal under a prescription.
#'
#' @param trees data.table with DBH (inches), X, Y (m), optional species/BasalA
#' @param area_m2 scaling area (occlusion-corrected)
#' @param method "none" | "below" | "above" | "species" | "spacing"
#' @param target_ba_ac residual basal area target (ft^2/ac) for below/above
#' @param remove_species species code removed when method = "species"
#' @param spacing_ft minimum spacing for the spacing/crop-tree method
#' @return logical vector: TRUE = marked for removal
#' @export
mark_removals <- function(trees, area_m2,
                          method = c("none", "below", "above", "species", "spacing"),
                          target_ba_ac = 80, remove_species = NULL,
                          spacing_ft = 14) {
  method <- match.arg(method)
  n <- nrow(trees)
  if (n == 0) return(logical())
  rm_flag <- rep(FALSE, n)
  if (method == "none") return(rm_flag)

  dbh_in <- suppressWarnings(as.numeric(trees$DBH))   # inches
  area_ac <- area_m2 * SQM_TO_HA * HA_TO_AC
  ba_ft2  <- stem_ba_ft2(trees)                       # per stem, ft^2

  if (method == "species") {
    if (is.null(remove_species) || !"species" %in% names(trees)) return(rm_flag)
    return(!is.na(trees$species) & trees$species == remove_species)
  }

  if (method == "spacing") {
    sp_m <- spacing_ft / M_TO_FT
    x <- suppressWarnings(as.numeric(trees$X))
    y <- suppressWarnings(as.numeric(trees$Y))
    ord <- order(-dbh_in)                 # keep the largest, crop-tree style
    keep_x <- numeric(0); keep_y <- numeric(0)
    keep <- rep(FALSE, n)
    for (i in ord) {
      if (is.na(x[i]) || is.na(y[i])) next
      if (length(keep_x) == 0 ||
          all(sqrt((keep_x - x[i])^2 + (keep_y - y[i])^2) >= sp_m)) {
        keep[i] <- TRUE
        keep_x <- c(keep_x, x[i]); keep_y <- c(keep_y, y[i])
      }
    }
    return(!keep)
  }

  # below / above: remove in size order until the residual BA target is met
  ord <- if (method == "below") order(dbh_in) else order(-dbh_in)
  ba_now <- sum(ba_ft2, na.rm = TRUE) / area_ac
  for (i in ord) {
    if (ba_now <= target_ba_ac) break
    rm_flag[i] <- TRUE
    ba_now <- ba_now - ba_ft2[i] / area_ac
  }
  rm_flag
}

# ---- diameter distribution + generative simulation ------------------------

#' Fit a two-parameter Weibull to observed diameters (method of moments).
#' Returns list(shape, scale); falls back to a broad default on bad input.
#' @export
fit_weibull <- function(dbh_in) {
  d <- dbh_in[!is.na(dbh_in) & dbh_in > 0]
  if (length(d) < 5) return(list(shape = 2, scale = max(4, mean(d, na.rm = TRUE))))
  m <- mean(d); s <- sd(d)
  if (!is.finite(s) || s <= 0) return(list(shape = 3, scale = m))
  cv <- s / m
  # Approximate inversion of the Weibull CV -> shape relation
  shape <- cv^(-1.086)
  shape <- min(max(shape, 0.8), 12)
  scale <- m / gamma(1 + 1 / shape)
  list(shape = shape, scale = scale)
}

#' Draw diameters (cm) from a fitted Weibull, truncated at a minimum.
#' @export
rweibull_dbh <- function(n, shape, scale, min_dbh = 1) {
  if (n <= 0) return(numeric(0))
  u <- runif(n)
  d <- scale * (-log(pmax(1e-9, u)))^(1 / shape)
  pmax(min_dbh, d)
}

#' Clark-Evans nearest-neighbour index: <1 clustered, ~1 random, >1 regular.
#' @export
clark_evans <- function(x, y, area_m2) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]; n <- length(x)
  if (n < 3 || is.na(area_m2) || area_m2 <= 0) return(NA_real_)
  nn <- vapply(seq_len(n), function(i) {
    d <- sqrt((x - x[i])^2 + (y - y[i])^2); d[i] <- Inf; min(d)
  }, numeric(1))
  expected <- 0.5 / sqrt(n / area_m2)
  mean(nn) / expected
}

#' Simulate a stem map over a rectangular AOI from fitted stand parameters.
#'
#' The output is SIMULATED, not measured: plot-scale structure is extrapolated
#' across the AOI. Label it as such wherever it is exported or displayed.
#'
#' @param width_m,height_m AOI dimensions
#' @param density_tph stems per hectare (use occlusion-corrected density)
#' @param shape,scale fitted Weibull diameter parameters (inches)
#' @param pattern "cluster" (Neyman-Scott), "random" (Poisson), "regular"
#' @param clump mean offspring per cluster (pattern = "cluster")
#' @param strata optional data.frame with `name` and `rel` (relative density);
#'        the AOI is split into equal vertical bands, one per stratum
#' @return data.table: x, y (m), DBH (in), H (m), cr, stratum
#' @export
simulate_stems <- function(width_m, height_m, density_tph, shape, scale,
                           pattern = c("cluster", "random", "regular"),
                           clump = 9, strata = NULL, seed = 7,
                           min_dbh = 1) {
  pattern <- match.arg(pattern)
  set.seed(seed)

  area_ha <- width_m * height_m * SQM_TO_HA
  n_target <- max(1L, round(density_tph * area_ha))

  if (is.null(strata) || nrow(as.data.frame(strata)) == 0) {
    strata <- data.frame(name = "All", rel = 1, stringsAsFactors = FALSE)
  }
  strata <- as.data.frame(strata)
  nb <- nrow(strata)
  band <- width_m / nb
  band_of <- function(x) pmin(nb, pmax(1L, floor(x / band) + 1L))

  if (pattern == "cluster") {
    n_par <- max(2L, round(n_target / max(1, clump)))
    px <- runif(n_par, 0, width_m); py <- runif(n_par, 0, height_m)
    xs <- numeric(0); ys <- numeric(0)
    for (i in seq_len(n_par)) {
      b <- band_of(px[i])
      k <- max(0L, round(clump * strata$rel[b] * runif(1, 0.5, 1.5)))
      if (k == 0) next
      cx <- px[i] + rnorm(k, 0, 6); cy <- py[i] + rnorm(k, 0, 6)
      keep <- cx >= 0 & cx <= width_m & cy >= 0 & cy <= height_m
      xs <- c(xs, cx[keep]); ys <- c(ys, cy[keep])
    }
  } else if (pattern == "regular") {
    cols <- max(1L, round(sqrt(n_target * width_m / height_m)))
    rows <- max(1L, ceiling(n_target / cols))
    sx <- width_m / cols; sy <- height_m / rows
    gx <- rep(seq_len(cols), times = rows)
    gy <- rep(seq_len(rows), each = cols)
    keep <- seq_len(min(length(gx), n_target))
    xs <- (gx[keep] - 0.5) * sx + runif(length(keep), -sx * 0.18, sx * 0.18)
    ys <- (gy[keep] - 0.5) * sy + runif(length(keep), -sy * 0.18, sy * 0.18)
    b <- band_of(xs)
    thin <- runif(length(xs)) <= strata$rel[b]
    xs <- xs[thin]; ys <- ys[thin]
  } else {
    xs <- runif(n_target, 0, width_m); ys <- runif(n_target, 0, height_m)
    b <- band_of(xs)
    thin <- runif(length(xs)) <= strata$rel[b]
    xs <- xs[thin]; ys <- ys[thin]
  }

  n <- length(xs)
  if (n == 0) return(data.table())

  dbh <- rweibull_dbh(n, shape, scale, min_dbh)      # inches
  d_cm <- dbh * IN_TO_CM
  ht  <- 1.37 + (28 * d_cm) / (d_cm + 16) * runif(n, 0.85, 1.15)   # metres
  cr  <- runif(n, 0.30, 0.58)

  data.table(
    x = xs, y = ys, DBH = dbh, H = ht, cr = cr,
    stratum = strata$name[band_of(xs)]
  )
}
